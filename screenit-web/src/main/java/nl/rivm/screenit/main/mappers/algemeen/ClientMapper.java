package nl.rivm.screenit.main.mappers.algemeen;

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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.dto.algemeen.BezwaarClientDto;
import nl.rivm.screenit.main.dto.algemeen.BrpGegevensDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactgegevensDto;
import nl.rivm.screenit.main.dto.algemeen.ClientDto;
import nl.rivm.screenit.main.dto.algemeen.TijdelijkAdresDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { OnderzoeksresultatenActieMapper.class })
public interface ClientMapper
{
	@Mappings({
		@Mapping(target = "bsn", source = "persoon.bsn"),
		@Mapping(target = "geboortedatum", source = "persoon.geboortedatum"),
		@Mapping(target = "briefDocument", source = "client", qualifiedByName = "getBriefDocument"),
	})
	BezwaarClientDto clientToBezwaarDto(Client client);

	@Named("getBriefDocument")
	default Long getBriefDocument(Client client)
	{
		var brpBezwaarMoment = client.getBezwaarMomenten().stream()
			.filter(bezwaarMoment -> bezwaarMoment.getBezwaren().stream().anyMatch(bezwaar -> bezwaar.getType() == BezwaarType.GEEN_OPNAME_UIT_BPR)).findFirst();
		return brpBezwaarMoment.map(bezwaarMoment -> bezwaarMoment.getBezwaarBrief().getId()).orElse(null);
	}

	@Mappings({
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "voornaam", source = "persoon.voornaam"),
		@Mapping(target = "achternaam", source = "persoon.achternaam"),
		@Mapping(target = "tussenvoegsel", source = "persoon.tussenvoegsel"),
		@Mapping(target = "geboortedatum", source = "persoon.geboortedatum"),
		@Mapping(target = "titel", source = "persoon.titel"),
		@Mapping(target = "partnerTussenvoegsel", source = "persoon.partnerTussenvoegsel"),
		@Mapping(target = "partnerAchternaam", source = "persoon.partnerAchternaam"),
		@Mapping(target = "geslacht", source = "persoon.geslacht"),
		@Mapping(target = "naamGebruik", source = "persoon.naamGebruik"),
		@Mapping(target = "bsn", source = "persoon.bsn"),
		@Mapping(target = "plaats", source = "persoon.gbaAdres.plaats"),
		@Mapping(target = "postcode", source = "persoon.gbaAdres.postcode"),
		@Mapping(target = "straat", source = "persoon.gbaAdres.straat"),
		@Mapping(target = "volledigeAdres", source = "persoon.gbaAdres", qualifiedByName = "getVolledigeAdres"),
		@Mapping(target = "overlijdensdatum", source = "persoon.overlijdensdatum"),
		@Mapping(target = "tijdelijkAdres", source = "client", qualifiedByName = "isTijdelijkAdres"),
		@Mapping(target = "tijdelijkAdresVolledig", source = "client", qualifiedByName = "getTijdelijkAdresVolledig"),
		@Mapping(target = "postadres", source = "client", qualifiedByName = "getPostadres"),
		@Mapping(target = "screeningsorganisatie", source = "persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam"),
		@Mapping(target = "actief", source = "client", qualifiedByName = "isActief"),
		@Mapping(target = "onderzoeksresultatenActies", source = "onderzoeksresultatenActies")
	})
	ClientDto clientToClientDto(Client client, @Context ClientService clientService);

	@Mappings({
		@Mapping(source = "startDatum", target = "begindatum"),
		@Mapping(source = "eindDatum", target = "einddatum"),
		@Mapping(source = "straat", target = "straatnaam"),
		@Mapping(source = "huisnummerAanduiding", target = "aanduidingBijHuisnummer"),
		@Mapping(target = "clientId", ignore = true)
	})
	TijdelijkAdresDto tijdelijkAdresToDto(TijdelijkAdres tijdelijkAdres);

	@Mappings({
		@Mapping(source = "begindatum", target = "startDatum"),
		@Mapping(source = "einddatum", target = "eindDatum"),
		@Mapping(source = "straatnaam", target = "straat"),
		@Mapping(source = "aanduidingBijHuisnummer", target = "huisnummerAanduiding"),
		@Mapping(target = "id", ignore = true),
		@Mapping(target = "soort", ignore = true),
		@Mapping(target = "geheimadres", ignore = true),
		@Mapping(target = "tijdelijk", ignore = true),
		@Mapping(target = "gemeente", ignore = true),
		@Mapping(target = "gemeentedeel", ignore = true),
		@Mapping(target = "land", ignore = true),
		@Mapping(target = "aanschrijfAdres", ignore = true),
		@Mapping(target = "gemeenteCode", ignore = true),
		@Mapping(target = "locatieBeschrijving", ignore = true),
		@Mapping(target = "postcodeCoordinaten", ignore = true)
	})
	TijdelijkAdres dtoToTijdelijkAdres(TijdelijkAdresDto dto);

	@Mappings({
		@Mapping(source = "begindatum", target = "startDatum"),
		@Mapping(source = "einddatum", target = "eindDatum"),
		@Mapping(source = "straatnaam", target = "straat"),
		@Mapping(source = "aanduidingBijHuisnummer", target = "huisnummerAanduiding"),
		@Mapping(target = "id", ignore = true),
		@Mapping(target = "soort", ignore = true),
		@Mapping(target = "geheimadres", ignore = true),
		@Mapping(target = "tijdelijk", ignore = true),
		@Mapping(target = "gemeente", ignore = true),
		@Mapping(target = "gemeentedeel", ignore = true),
		@Mapping(target = "land", ignore = true),
		@Mapping(target = "aanschrijfAdres", ignore = true),
		@Mapping(target = "gemeenteCode", ignore = true),
		@Mapping(target = "locatieBeschrijving", ignore = true),
		@Mapping(target = "postcodeCoordinaten", ignore = true)
	})
	void updateTijdelijkAdres(@MappingTarget TijdelijkAdres target, TijdelijkAdresDto dto);

	@Named("isTijdelijkAdres")
	default boolean isTijdelijkAdres(Client client)
	{
		return client.getPersoon().getTijdelijkAdres() != null;
	}

	@Named("getTijdelijkAdresVolledig")
	default String getTijdelijkAdresVolledig(Client client)
	{
		var tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			return null;
		}
		var vandaag = LocalDate.now();
		var startDatum = tijdelijkAdres.getStartDatum();
		if (startDatum != null && DateUtil.toLocalDate(startDatum).isAfter(vandaag))
		{
			return null;
		}
		var eindDatum = tijdelijkAdres.getEindDatum();
		if (eindDatum != null && DateUtil.toLocalDate(eindDatum).isBefore(vandaag))
		{
			return null;
		}
		return formatAdres(tijdelijkAdres);
	}

	@Named("getPostadres")
	default String getPostadres(Client client)
	{
		var adres = AdresUtil.getAdres(client.getPersoon(), LocalDate.now());
		if (adres == null)
		{
			return null;
		}
		return formatAdres(adres);
	}

	private String formatAdres(Adres adres)
	{
		var parts = new java.util.ArrayList<String>();
		if (StringUtils.isNotBlank(adres.getStraat()))
		{
			var straatHuisnummer = adres.getStraat();
			var huisnummerVolledig = AdresUtil.getHuisnummerVolledig(adres);
			if (StringUtils.isNotBlank(huisnummerVolledig))
			{
				straatHuisnummer += " " + huisnummerVolledig;
			}
			parts.add(straatHuisnummer);
		}
		if (StringUtils.isNotBlank(adres.getPostcode()))
		{
			parts.add(adres.getPostcode());
		}
		if (StringUtils.isNotBlank(adres.getPlaats()))
		{
			parts.add(adres.getPlaats());
		}
		return parts.isEmpty() ? null : String.join(", ", parts);
	}

	@Named("getVolledigeAdres")
	default String getVolledigeAdres(BagAdres adres)
	{
		if (adres == null)
		{
			return null;
		}
		var volledigeAdres = adres.getAdres();
		return StringUtils.isBlank(volledigeAdres) ? null : volledigeAdres;
	}

	@Mappings({
		@Mapping(target = "clientId", source = "id"),
		@Mapping(target = "voornaam", source = "persoon.voornaam"),
		@Mapping(target = "achternaam", source = "persoon.achternaam"),
		@Mapping(target = "tussenvoegsel", source = "persoon.tussenvoegsel"),
		@Mapping(target = "aanspreekvorm", source = "persoon.aanhef"),
		@Mapping(target = "titel", source = "persoon.titel"),
		@Mapping(target = "geboortedatum", source = "persoon.geboortedatum"),
		@Mapping(target = "bsn", source = "persoon.bsn"),
		@Mapping(target = "naamGebruik", source = "persoon.naamGebruik"),
		@Mapping(target = "partnerTussenvoegsel", source = "persoon.partnerTussenvoegsel"),
		@Mapping(target = "partnerAchternaam", source = "persoon.partnerAchternaam"),
		@Mapping(target = "geslacht", source = "persoon.geslacht"),
		@Mapping(target = "overlijdensdatum", source = "persoon.overlijdensdatum"),
		@Mapping(target = "mobielNummer", source = "persoon.telefoonnummer1"),
		@Mapping(target = "extraNummer", source = "persoon.telefoonnummer2"),
		@Mapping(target = "emailAdres", source = "persoon.emailadres"),
		@Mapping(target = "heeftMammaAfspraak", source = "client", qualifiedByName = "heeftOpenMammaAfspraak"),
		@Mapping(target = "doelgroepen", ignore = true),
		@Mapping(target = "dubbeleTijdReden", ignore = true),
	})
	ClientContactgegevensDto clientToClientContactgegevensDto(Client client, @Context ClientContactService clientContactService);

	@Named("heeftOpenMammaAfspraak")
	default boolean heeftOpenMammaAfspraak(Client client, @Context ClientContactService clientContactService)
	{
		return clientContactService.heeftOpenMammaAfspraak(client);
	}

	@Named("isActief")
	default boolean isActief(Client client, @Context ClientService clientService)
	{
		return clientService.isClientActief(client);
	}

	@Mappings({
		@Mapping(target = "indicatieStatus", source = "gbaStatus"),
		@Mapping(target = "datumLaatsteBrpMutatie", source = "laatsteGbaMutatie.mutatieDatum"),
		@Mapping(target = "laatstAangevraagdOp", source = "gbaVragen", qualifiedByName = "getDatumLaatsteAanvraag"),
		@Mapping(target = "tijdelijkBrpAdres", source = "persoon.tijdelijkGbaAdres", qualifiedByName = "isTijdelijkGbaAdres"),
		@Mapping(target = "datumVertrokkenUitNederland", source = "persoon.datumVertrokkenUitNederland"),
	})
	BrpGegevensDto clientToBrpDto(Client client);

	@Mappings({
		@Mapping(target = "clientId", source = "id"),
		@Mapping(target = "straatnaam", source = "persoon.tijdelijkGbaAdres.straat"),
		@Mapping(target = "huisnummer", source = "persoon.tijdelijkGbaAdres.huisnummer"),
		@Mapping(target = "huisletter", source = "persoon.tijdelijkGbaAdres.huisletter"),
		@Mapping(target = "huisnummerToevoeging", source = "persoon.tijdelijkGbaAdres.huisnummerToevoeging"),
		@Mapping(target = "aanduidingBijHuisnummer", source = "persoon.tijdelijkGbaAdres.huisnummerAanduiding"),
		@Mapping(target = "postcode", source = "persoon.tijdelijkGbaAdres.postcode"),
		@Mapping(target = "plaats", source = "persoon.tijdelijkGbaAdres.plaats"),
		@Mapping(target = "begindatum", ignore = true),
		@Mapping(target = "einddatum", ignore = true),
	})
	TijdelijkAdresDto clientToBrpTijdelijkAdres(Client client);

	@Named("getDatumLaatsteAanvraag")
	default LocalDateTime getDatumLaatsteAanvraag(List<GbaVraag> vragen)
	{
		return vragen.stream()
			.max(Comparator.comparing(GbaVraag::getDatum))
			.map(GbaVraag::getDatum)
			.orElse(null);
	}

	@Named("isTijdelijkGbaAdres")
	default Boolean isTijdelijkGbaAdres(TijdelijkGbaAdres adres)
	{
		return adres != null;
	}
}
