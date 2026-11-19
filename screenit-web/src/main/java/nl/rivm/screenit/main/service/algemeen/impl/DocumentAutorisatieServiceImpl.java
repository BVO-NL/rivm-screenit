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

import java.util.Optional;
import java.util.regex.Pattern;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.service.algemeen.DocumentAutorisatieService;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.BezwaarMomentRepository;
import nl.rivm.screenit.repository.algemeen.OnderzoeksresultatenActieRepository;
import nl.rivm.screenit.repository.algemeen.OverdrachtPersoonsgegevensRepository;
import nl.rivm.screenit.repository.mamma.MammaVisitatieRepository;
import nl.rivm.screenit.service.AutorisatieService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class DocumentAutorisatieServiceImpl implements DocumentAutorisatieService
{
	private static final String ID_PLACEHOLDER = "{id}";

	private static final Pattern HANDLEIDING_PATH = maakPattern(FileStoreLocation.ALGEMEEN_HANDLEIDINGEN);

	private static final Pattern BEZWAAR_PATH = maakPattern(FileStoreLocation.BEZWAAR);

	private static final Pattern OVERDRACHT_PERSOONSGEGEVENS_PATH = maakPattern(FileStoreLocation.OVERDRACHT_PERSOONSGEGEVENS);

	private static final Pattern VISITATIE_BIJLAGE_PATH = maakPattern(FileStoreLocation.MAMMA_VISITATIE_BIJLAGE);

	private final AutorisatieService autorisatieService;

	private final BezwaarMomentRepository bezwaarMomentRepository;

	private final OnderzoeksresultatenActieRepository onderzoeksresultatenActieRepository;

	private final OverdrachtPersoonsgegevensRepository overdrachtPersoonsgegevensRepository;

	private final MammaVisitatieRepository visitatieRepository;

	@Override
	public boolean magDownloaden(UploadDocument document, OrganisatieMedewerker medewerker)
	{
		var path = document.getPath();
		if (path == null || medewerker == null)
		{
			return false;
		}
		if (BEZWAAR_PATH.matcher(path).matches())
		{
			return magBezwaarDocumentDownloaden(document, medewerker);
		}
		if (OVERDRACHT_PERSOONSGEGEVENS_PATH.matcher(path).matches())
		{
			return magOverdrachtDocumentDownloaden(document, medewerker);
		}
		if (VISITATIE_BIJLAGE_PATH.matcher(path).matches())
		{
			return magVisitatieBijlageDownloaden(document, medewerker);
		}
		if (HANDLEIDING_PATH.matcher(path).matches())
		{
			return autorisatieService.getToegangLevel(medewerker, Actie.INZIEN, true, Recht.HANDLEIDINGEN) != null;
		}

		return false;
	}

	private boolean magBezwaarDocumentDownloaden(UploadDocument document, OrganisatieMedewerker medewerker)
	{
		var bezwaarMoment = bezwaarMomentRepository.findByBezwaarBriefId(document.getId());
		if (bezwaarMoment.isPresent())
		{
			var moment = bezwaarMoment.get();
			var rechten = heeftBrpBezwaar(moment) ? new Recht[] { Recht.MEDEWERKER_BEZWAAR_BRP, Recht.MEDEWERKER_CLIENT_BEZWAAR }
				: new Recht[] { Recht.MEDEWERKER_CLIENT_BEZWAAR };
			return magClientDocumentInzien(moment.getClient(), medewerker, rechten);
		}

		return onderzoeksresultatenActieRepository.findByGetekendeBriefId(document.getId())
			.map(actie -> magClientDocumentInzien(actie.getClient(), medewerker, Recht.MEDEWERKER_CLIENT_BEZWAAR))
			.orElse(false);
	}

	private boolean magOverdrachtDocumentDownloaden(UploadDocument document, OrganisatieMedewerker medewerker)
	{

		return overdrachtPersoonsgegevensRepository.findByOntvangenAanvraagbriefId(document.getId()).map(
				overdrachtPersoonsgegevens -> magClientDocumentInzien(overdrachtPersoonsgegevens.getClient(), medewerker, Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
					Recht.MEDEWERKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS))
			.orElseGet(() -> overdrachtPersoonsgegevensRepository.findByGegenereerdeDataDumpId(document.getId())
				.map(overdracht -> magClientDocumentInzien(overdracht.getClient(), medewerker, Recht.MEDEWERKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS))
				.orElse(false));
	}

	private boolean magVisitatieBijlageDownloaden(UploadDocument document, OrganisatieMedewerker medewerker)
	{
		return visitatieRepository.existsByRapportageBijlageIdOrVragenlijstBijlageId(document.getId(), document.getId())
			&& autorisatieService.getToegangLevel(medewerker, Actie.INZIEN, true, Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK) != null;
	}

	private boolean heeftBrpBezwaar(BezwaarMoment bezwaarMoment)
	{
		return bezwaarMoment.getBezwaren().stream().anyMatch(bezwaar -> bezwaar.getType() == BezwaarType.GEEN_OPNAME_UIT_BPR);
	}

	private boolean magClientDocumentInzien(Client client, OrganisatieMedewerker medewerker, Recht... rechten)
	{
		var toegangLevel = autorisatieService.getToegangLevel(medewerker, Actie.INZIEN, true, rechten);
		if (toegangLevel == null)
		{
			return false;
		}
		return toegangLevel != ToegangLevel.REGIO || valtBinnenRegioVanMedewerker(client, medewerker);
	}

	private boolean valtBinnenRegioVanMedewerker(Client client, OrganisatieMedewerker medewerker)
	{
		return Optional.ofNullable(client.getPersoon().getGbaAdres())
			.map(BagAdres::getGbaGemeente)
			.map(Gemeente::getScreeningOrganisatie)
			.map(screeningOrganisatie -> screeningOrganisatie.equals(medewerker.getOrganisatie()))
			.orElse(false);
	}

	private static Pattern maakPattern(FileStoreLocation location)
	{
		var delen = location.getPath().split(Pattern.quote(ID_PLACEHOLDER), -1);
		var regex = new StringBuilder();
		for (var index = 0; index < delen.length; index++)
		{
			if (index > 0)
			{
				regex.append("\\d+");
			}
			regex.append(Pattern.quote(delen[index]));
		}
		return Pattern.compile(regex.append(".*").toString());
	}
}
