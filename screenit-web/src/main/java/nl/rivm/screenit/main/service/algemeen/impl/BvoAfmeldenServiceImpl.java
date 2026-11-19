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
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.BaseAfmeldenDto;
import nl.rivm.screenit.main.dto.cervix.CervixAfmeldenDto;
import nl.rivm.screenit.main.service.algemeen.BvoAfmeldenService;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.AfmeldingUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Service
@RequiredArgsConstructor
public class BvoAfmeldenServiceImpl implements BvoAfmeldenService
{
	private static final List<ClientContactManier> DEFINITIEF_AFMELDEN_MANIEREN = List.of(ClientContactManier.FORMULIER_VOOR_AANVRAGEN, ClientContactManier.DIRECT);

	private final ClientContactService clientContactService;

	private final UploadDocumentService uploadDocumentService;

	@Override
	public List<AfmeldingType> getAfmeldOpties(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		return clientContactService.getAvailableAfmeldopties(client, bevolkingsonderzoek, false);
	}

	@Override
	@Transactional
	public void afmelden(Client client, Bevolkingsonderzoek bevolkingsonderzoek, BaseAfmeldenDto afmelden, MultipartFile handtekeningBestand,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker) throws IOException
	{
		AfmeldingUtil.valideerAfmeldType(afmelden.getType(), getAfmeldOpties(client, bevolkingsonderzoek));
		var afmelding = maakAfmelding(bevolkingsonderzoek);
		afmelding.setType(afmelden.getType());
		if (afmelden.getType() == AfmeldingType.EENMALIG)
		{
			afmelding.setManier(ClientContactManier.DIRECT);
		}
		else
		{
			vulDefinitieveAfmelding(afmelding, afmelden, handtekeningBestand);
		}
		clientContactService.afmelden(client, afmelding, afmelden.getNotitie(), ingelogdeOrganisatieMedewerker);
	}

	private static Afmelding<?, ?, ?> maakAfmelding(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		return switch (bevolkingsonderzoek)
		{
			case CERVIX -> new CervixAfmelding();
			default -> throw new IllegalArgumentException("Afmelden via het contactscherm is nog niet beschikbaar voor " + bevolkingsonderzoek);
		};
	}

	private void vulDefinitieveAfmelding(Afmelding<?, ?, ?> afmelding, BaseAfmeldenDto afmelden, MultipartFile handtekeningBestand) throws IOException
	{
		if (afmelden.getManier() == null || !DEFINITIEF_AFMELDEN_MANIEREN.contains(afmelden.getManier()))
		{
			throw new IllegalStateException("error.afmelden.manier.verplicht");
		}
		afmelding.setManier(afmelden.getManier());
		if (afmelden.getManier() == ClientContactManier.FORMULIER_VOOR_AANVRAGEN)
		{

			zetStandaardReden(afmelding);
			return;
		}
		zetReden(afmelding, afmelden);
		valideerPdfBestand(handtekeningBestand);
		afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
		afmelding.setHandtekeningDocumentAfmelding(uploadDocumentService.multipartToUploadDocument(handtekeningBestand));
	}

	private static void zetStandaardReden(Afmelding<?, ?, ?> afmelding)
	{
		if (afmelding instanceof CervixAfmelding cervixAfmelding)
		{
			cervixAfmelding.setReden(CervixAfmeldingReden.ANDERS);
		}
	}

	private static void zetReden(Afmelding<?, ?, ?> afmelding, BaseAfmeldenDto afmelden)
	{
		if (afmelding instanceof CervixAfmelding cervixAfmelding && afmelden instanceof CervixAfmeldenDto cervixAfmeldenDto)
		{
			if (cervixAfmeldenDto.getReden() == null)
			{
				throw new IllegalStateException("error.afmelden.reden.verplicht");
			}
			cervixAfmelding.setReden(cervixAfmeldenDto.getReden());
		}
	}

	private static void valideerPdfBestand(MultipartFile bestand)
	{
		if (bestand == null || bestand.isEmpty())
		{
			throw new IllegalStateException("error.bestand.verplicht");
		}
		if (!FileType.PDF.getAllowedContentTypes().contains(bestand.getContentType()))
		{
			throw new IllegalStateException("error.bestandtype.niet.toegestaan");
		}
	}
}
