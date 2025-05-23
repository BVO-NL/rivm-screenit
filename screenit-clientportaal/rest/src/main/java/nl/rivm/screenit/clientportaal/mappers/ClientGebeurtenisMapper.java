package nl.rivm.screenit.clientportaal.mappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.clientportaal.model.ClientGebeurtenisDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.ClientGebeurtenis;

import org.mapstruct.AfterMapping;
import org.mapstruct.InheritInverseConfiguration;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class)
public interface ClientGebeurtenisMapper
{

	@Mappings({
		@Mapping(source = "datum", target = "datumTijd"),
		@Mapping(source = "extraParam", target = "extraParameters"),
		@Mapping(source = "type", target = "tekstKey")
	})
	ClientGebeurtenisDto clientGebeurtenisToDto(ClientGebeurtenis clientGebeurtenis);

	@InheritInverseConfiguration
	ClientGebeurtenis dtoToClientGebeurtenis(ClientGebeurtenisDto dto);

	List<ClientGebeurtenisDto> clientGebeurtenisListToDtoList(List<ClientGebeurtenis> clientGebeurtenisList);

	List<ClientGebeurtenis> dtoListToClientGebeurtenisList(List<ClientGebeurtenisDto> dtoList);

	@AfterMapping
	static void sortPOJOs(@MappingTarget List<ClientGebeurtenis> clientGebeurtenisList)
	{
		clientGebeurtenisList.sort(Comparator.comparing(ClientGebeurtenis::getDatum).reversed());
	}

	@AfterMapping
	static void sortDTOs(@MappingTarget List<ClientGebeurtenisDto> dtoList)
	{
		dtoList.sort(Comparator.comparing(ClientGebeurtenisDto::getDatumTijd).reversed());
	}

}
