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

import java.util.List;

import nl.rivm.screenit.main.dto.algemeen.ClientContactActieDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { MedewerkerMapper.class })
public interface ClientContactMapper
{
	@Mappings({
		@Mapping(target = "datumTijd", source = "datum"),
		@Mapping(target = "medewerker", source = "organisatieMedewerker.medewerker"),
		@Mapping(target = "notitie", source = "opmerking"),
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "clientId", source = "client.id"),
		@Mapping(target = "bevolkingsonderzoeken", source = "contact", qualifiedByName = "getBevolkingsonderzoeken"),
		@Mapping(target = "acties", source = "contact", qualifiedByName = "getActies"),
	})
	ClientContactDto clientContactNaarDto(ClientContact contact);

	ClientContactActieDto clientContactActieNaarDto(ClientContactActie actie);

	@Named("getBevolkingsonderzoeken")
	default List<Bevolkingsonderzoek> getBevolkingsonderzoeken(ClientContact contact)
	{
		return contact.getActies().stream()
			.flatMap(actie -> actie.getType().getBevolkingsonderzoeken().stream())
			.distinct()
			.toList();
	}

	@Named("getActies")
	default List<ClientContactActieDto> getActies(ClientContact contact)
	{
		return contact.getActies().stream().map(this::clientContactActieNaarDto).toList();
	}
}
