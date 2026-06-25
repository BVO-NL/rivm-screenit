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

import nl.rivm.screenit.main.dto.algemeen.BezwaarClientDto;
import nl.rivm.screenit.main.dto.algemeen.ClientDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.service.ClientService;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
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
		@Mapping(target = "screeningsorganisatie", source = "persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam"),
		@Mapping(target = "actief", source = "client", qualifiedByName = "isActief")

	})
	ClientDto clientToClientDto(Client client, @Context ClientService clientService);

	@Named("isTijdelijkAdres")
	default boolean isTijdelijkAdres(Client client)
	{
		return client.getPersoon().getTijdelijkAdres() != null;
	}

	@Named("getVolledigeAdres")
	default String getVolledigeAdres(BagAdres adres)
	{
		if (adres == null)
		{
			return null;
		}
		return "%s %s".formatted(adres.getStraat(), adres.getVolledigeHuisaanduiding());
	}

	@Named("isActief")
	default boolean isActief(Client client, @Context ClientService clientService)
	{
		return clientService.isClientActief(client);
	}
}
