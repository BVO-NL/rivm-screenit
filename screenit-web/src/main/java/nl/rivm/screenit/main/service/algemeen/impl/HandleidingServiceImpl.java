package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.EntityManager;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.algemeen.HandleidingDto;
import nl.rivm.screenit.main.dto.algemeen.HandleidingUploadResultDto;
import nl.rivm.screenit.main.exception.BestandNietGevondenException;
import nl.rivm.screenit.main.exception.BestandsnaamInGebruikException;
import nl.rivm.screenit.main.exception.OngeldigeBestandsTypeException;
import nl.rivm.screenit.main.service.algemeen.HandleidingService;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.repository.algemeen.UploadDocumentRepository;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.multipart.MultipartFile;

import static nl.rivm.screenit.specification.algemeen.UploadDocumentSpecification.heeftNaamDieEindigtOp;
import static nl.rivm.screenit.specification.algemeen.UploadDocumentSpecification.heeftPathDieStartMet;

@Slf4j
@Service
@RequiredArgsConstructor
public class HandleidingServiceImpl implements HandleidingService
{
	private final UploadDocumentRepository uploadDocumentRepository;

	private final EntityManager entityManager;

	private final UploadDocumentService uploadDocumentService;

	@Override
	public List<HandleidingDto> getHandleidingen()
	{
		var handleidingenPad = FileStoreLocation.ALGEMEEN_HANDLEIDINGEN.getPath();
		var spec = heeftPathDieStartMet(handleidingenPad);
		var handleidingen = uploadDocumentRepository.findAll(spec);

		var resultaat = new ArrayList<HandleidingDto>();

		for (var handleiding : handleidingen)
		{
			var handleidingDto = new HandleidingDto();
			handleidingDto.setId(handleiding.getId());
			handleidingDto.setBestandsnaam(handleiding.getNaam());
			handleidingDto.setBestandspad(handleiding.getPath());

			var revisions = EntityAuditUtil.getEntityHistory(handleiding, entityManager, null, false, 1);
			var revisionInfo = revisions.isEmpty() ? null : EntityAuditUtil.getRevisionInfo(revisions.getFirst());

			if (revisionInfo != null)
			{
				handleidingDto.setBewerkingsdatum(DateUtil.toLocalDateTime(new Date(revisionInfo.getTimestamp())));

				var orgMedewerker = revisionInfo.getOrganisatieMedewerker();
				handleidingDto.setLaatstGewijzigdDoor(orgMedewerker.getMedewerker().getGebruikersnaam());
			}
			resultaat.add(handleidingDto);
		}
		return resultaat;
	}

	@Override
	@Transactional
	public List<HandleidingUploadResultDto> uploadHandleidingen(List<MultipartFile> bestanden, List<String> bestandsnamen)
	{
		var resultaten = new ArrayList<HandleidingUploadResultDto>();

		for (int i = 0; i < bestanden.size(); i++)
		{
			var bestandsnaam = bestandsnamen.get(i);
			try
			{
				slaHandleidingOp(bestanden.get(i), bestandsnaam);
				resultaten.add(maakResultaatDto(bestandsnaam, true, null));
			}
			catch (BestandsnaamInGebruikException | OngeldigeBestandsTypeException e)
			{
				resultaten.add(maakResultaatDto(bestandsnaam, false, e.getMessage()));
			}
			catch (IOException e)
			{
				LOG.error("Fout bij het uploaden van handleiding '{}'", bestandsnaam, e);
				resultaten.add(maakResultaatDto(bestandsnaam, false, "Fout bij het opslaan van het bestand"));
			}
		}

		return resultaten;
	}

	@Override
	@Transactional
	public HandleidingUploadResultDto bewerkHandleiding(Long id, MultipartFile bestand, String bestandsnaam)
	{
		if ((bestand == null || bestand.isEmpty()) && StringUtils.isBlank(bestandsnaam))
		{
			return maakResultaatDto(null, true, null);
		}

		var document = zoekBestaandeHandleiding(id);
		var nieuweBestandsnaam = StringUtils.isNotBlank(bestandsnaam) ? bestandsnaam : document.getNaam();

		if (bestand == null || bestand.isEmpty())
		{
			return werkNaamBij(document, nieuweBestandsnaam);
		}

		vervangHandleiding(document, bestand, nieuweBestandsnaam);
		return maakResultaatDto(nieuweBestandsnaam, true, null);
	}

	private void vervangHandleiding(UploadDocument document, MultipartFile bestand, String nieuweBestandsnaam)
	{
		if (!nieuweBestandsnaam.equals(document.getNaam()) && isNaamAlInGebruik(nieuweBestandsnaam))
		{
			LOG.warn("Handleiding '{}' bestaat al als handleiding.", nieuweBestandsnaam);
			throw new BestandsnaamInGebruikException(nieuweBestandsnaam + " is al in gebruik");
		}

		uploadDocumentService.delete(document);
		LOG.info("Oude handleiding '{}' verwijderd", document.getNaam());

		try
		{
			slaHandleidingOp(bestand, nieuweBestandsnaam);
		}
		catch (MaxUploadSizeExceededException e)
		{
			LOG.error("Handleiding '{}' is te groot om te uploaden", nieuweBestandsnaam, e);
			throw new OngeldigeBestandsTypeException(nieuweBestandsnaam + " is te groot om te uploaden");
		}
		catch (IOException e)
		{
			LOG.error("Fout bij het opslaan van handleiding '{}'", nieuweBestandsnaam, e);
			throw new RuntimeException("Fout bij het opslaan van het bestand", e);
		}
	}

	private void slaHandleidingOp(MultipartFile bestand, String bestandsnaam) throws IOException
	{
		if (!FileType.PDF.getAllowedContentTypes().contains(bestand.getContentType()))
		{
			LOG.warn("{} is geen PDF (content type: {})", bestandsnaam, bestand.getContentType());
			throw new OngeldigeBestandsTypeException(bestandsnaam + " is geen PDF-bestand");
		}

		if (isNaamAlInGebruik(bestandsnaam))
		{
			LOG.warn("Bestand '{}' bestaat al de filestore", bestandsnaam);
			throw new BestandsnaamInGebruikException(bestandsnaam + " bestaat al");
		}

		var uploadDocument = uploadDocumentService.multipartToUploadDocument(bestand);
		uploadDocument.setNaam(bestandsnaam);
		uploadDocument.setContentType(MediaType.APPLICATION_PDF_VALUE);

		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.ALGEMEEN_HANDLEIDINGEN);

		LOG.info("Handleiding {} succesvol geüpload", bestandsnaam);
	}

	@Override
	@Transactional
	public HandleidingUploadResultDto verwijderHandleiding(Long id)
	{
		var document = zoekBestaandeHandleiding(id);

		uploadDocumentService.delete(document);

		return maakResultaatDto(document.getNaam(), true, document.getNaam() + " succesvol verwijderd");
	}

	private UploadDocument zoekBestaandeHandleiding(Long id)
	{
		var handleidingenPad = FileStoreLocation.ALGEMEEN_HANDLEIDINGEN.getPath();
		return uploadDocumentRepository.findById(id)
			.filter(doc -> doc.getPath() != null && doc.getPath().toLowerCase().startsWith(handleidingenPad.toLowerCase()))
			.orElseThrow(() ->
			{
				LOG.warn("Handleiding met id '{}' niet gevonden in de handleidingen", id);
				return new BestandNietGevondenException("Handleiding niet gevonden");
			});
	}

	private HandleidingUploadResultDto werkNaamBij(UploadDocument document, String bestandsnaam)
	{
		if (bestandsnaam.equals(document.getNaam()))
		{
			return maakResultaatDto(bestandsnaam, true, null);
		}

		if (isNaamAlInGebruik(bestandsnaam))
		{
			LOG.warn("Bestandsnaam '{}' is al in gebruik in de handleidingen filestore", bestandsnaam);
			throw new BestandsnaamInGebruikException(bestandsnaam + " is al in gebruik");
		}

		document.setNaam(bestandsnaam);
		LOG.info("Naam van handleiding bijgewerkt naar '{}'", bestandsnaam);
		return maakResultaatDto(bestandsnaam, true, null);
	}

	private HandleidingUploadResultDto maakResultaatDto(String bestandsnaam, boolean geslaagd, String foutmelding)
	{
		var resultaat = new HandleidingUploadResultDto();
		resultaat.setBestandsnaam(bestandsnaam);
		resultaat.setGeslaagd(geslaagd);
		resultaat.setFoutmelding(foutmelding);
		return resultaat;
	}

	private boolean isNaamAlInGebruik(String bestandsnaam)
	{
		return uploadDocumentRepository.exists(
			heeftPathDieStartMet(FileStoreLocation.ALGEMEEN_HANDLEIDINGEN.getPath())
				.and(heeftNaamDieEindigtOp(bestandsnaam))
		);
	}
}
