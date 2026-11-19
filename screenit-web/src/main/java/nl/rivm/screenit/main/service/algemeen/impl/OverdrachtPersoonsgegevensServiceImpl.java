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
import java.io.OutputStream;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.algemeen.OverdrachtType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.OverdrachtPersoonsgegevensRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.algemeen.OverdrachtPersoonsgegevensSpecification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Service
@Transactional
@AllArgsConstructor
public class OverdrachtPersoonsgegevensServiceImpl implements OverdrachtPersoonsgegevensService
{
	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BaseBriefService briefService;

	private final UploadDocumentService uploadDocumentService;

	private final LogService logService;

	private final OverdrachtPersoonsgegevensRepository overdrachtPersoonsgegevensRepository;

	private final BaseOverdrachtPersoonsgegevensService baseOverdrachtPersoonsgegevensService;

	@Override
	public boolean heeftOpenstaandVerzoek(Client client)
	{
		return overdrachtPersoonsgegevensRepository.exists(OverdrachtPersoonsgegevensSpecification.heeftOpenstaandVerzoek(client));
	}

	@Override
	public Optional<OverdrachtPersoonsgegevens> findOpenstaandVerzoek(Client client)
	{
		return overdrachtPersoonsgegevensRepository.findOne(OverdrachtPersoonsgegevensSpecification.heeftOpenstaandVerzoek(client));
	}

	@Override
	public Optional<OverdrachtPersoonsgegevens> findOverdracht(Client client, Long overdrachtId)
	{
		return overdrachtPersoonsgegevensRepository.findOne(OverdrachtPersoonsgegevensSpecification.heeftClient(client).and(heeftId(overdrachtId)));
	}

	@Override
	public List<OverdrachtPersoonsgegevens> findOverdrachten(Client client)
	{
		return overdrachtPersoonsgegevensRepository.findAll(OverdrachtPersoonsgegevensSpecification.heeftClient(client));
	}

	@Override
	@Transactional(rollbackFor = IOException.class)
	public void verwerkKeuzesViaClientportaal(Client client, OverdrachtPersoonsgegevens keuzes, OrganisatieMedewerker medewerker) throws IOException
	{
		keuzes.setType(OverdrachtType.CLIENTPORTAAL);
		baseOverdrachtPersoonsgegevensService.maakOverdrachtVerzoek(client, keuzes, medewerker);
		logService.logGebeurtenis(LogGebeurtenis.CLIENT_OVERDRACHT_PERSOONSGEGEVENS_GEUPLOAD, medewerker, client);
	}

	@Override
	public void verwerkKeuzesViaMedewerker(OverdrachtPersoonsgegevens openstaandVerzoek, OverdrachtPersoonsgegevens keuzes, MultipartFile bestand, OrganisatieMedewerker medewerker)
		throws IOException
	{
		openstaandVerzoek.setBkGegevens(keuzes.getBkGegevens());
		openstaandVerzoek.setBkBeelden(keuzes.getBkBeelden());
		openstaandVerzoek.setBmhkGegevens(keuzes.getBmhkGegevens());
		openstaandVerzoek.setDkGegevens(keuzes.getDkGegevens());
		if (bestand != null)
		{
			var uploadDocument = uploadDocumentService.multipartToUploadDocument(bestand);
			slaOntvangenFormulierOp(openstaandVerzoek, uploadDocument, medewerker);
		}
		hibernateService.saveOrUpdate(openstaandVerzoek);
	}

	@Override
	public void slaOntvangenFormulierOp(OverdrachtPersoonsgegevens overdracht, UploadDocument uploadDocument, Account account) throws IOException
	{
		if (overdracht.getOntvangenAanvraagbrief() == null)
		{
			saveOrUpdateGetekendFormulier(overdracht, uploadDocument);
			overdracht.setOntvangenAanvraagbrief(uploadDocument);
			overdracht.setStatus(AanvraagBriefStatus.BRIEF_ONTVANGEN);
			overdracht.setStatusDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(overdracht);
			logService.logGebeurtenis(LogGebeurtenis.CLIENT_OVERDRACHT_PERSOONSGEGEVENS_GEUPLOAD, account, overdracht.getClient());
		}
		else
		{
			vervangGetekendeFormulier(overdracht, uploadDocument, account);
		}
	}

	private void vervangGetekendeFormulier(OverdrachtPersoonsgegevens overdracht, UploadDocument nieuwDocument, Account account) throws IOException
	{
		uploadDocumentService.delete(overdracht.getOntvangenAanvraagbrief());
		overdracht.setOntvangenAanvraagbrief(nieuwDocument);
		saveOrUpdateGetekendFormulier(overdracht, nieuwDocument);
		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, account, overdracht.getClient(), "Formulier inzage/overdracht persoonsgegevens is vervangen.");
	}

	@Override
	public void verstuurGeenHandtekeningBrief(OverdrachtPersoonsgegevens overdracht, Account ingelogdAccount)
	{
		var brief = briefService.maakAlgemeneBrief(overdracht.getClient(), BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_HANDTEKENING);
		overdracht.setGeenHandtekeningBrief(brief);
		overdracht.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(overdracht);
	}

	private void saveOrUpdateGetekendFormulier(OverdrachtPersoonsgegevens overdracht, UploadDocument uploadDocument) throws IOException
	{
		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.OVERDRACHT_PERSOONSGEGEVENS, overdracht.getClient().getId());
	}

	@Override
	public void afronden(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens)
	{
		overdrachtPersoonsgegevens.setStatus(AanvraagBriefStatus.VERWERKT);
		overdrachtPersoonsgegevens.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(overdrachtPersoonsgegevens);
	}

	@Override
	public void vervangFormulier(OverdrachtPersoonsgegevens overdracht, MultipartFile bestand, Account account) throws IOException
	{
		slaOntvangenFormulierOp(overdracht, uploadDocumentService.multipartToUploadDocument(bestand), account);
	}

	@Override
	public void maakExcel(OverdrachtPersoonsgegevens overdracht, OutputStream outputStream, Account account)
	{
		baseOverdrachtPersoonsgegevensService.maakExcel(overdracht, outputStream, account);
	}
}
