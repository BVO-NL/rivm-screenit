package nl.rivm.screenit.main.mappers.mamma;

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

import nl.rivm.screenit.main.model.mamma.dto.MammaVisitatieDto;
import nl.rivm.screenit.mappers.OrganisatieMapper;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.mamma.MammaVisitatie;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class, uses = { OrganisatieMapper.class })
public interface MammaVisitatieMapper
{
	@Mappings({
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "omschrijving", source = "omschrijving"),
		@Mapping(target = "gestartOp", source = "gestartOp"),
		@Mapping(target = "afgerondOp", source = "afgerondOp"),
		@Mapping(target = "aangemaaktOp", source = "aangemaaktOp"),
		@Mapping(target = "aangemaaktDoor", source = "aangemaaktDoor.medewerker.achternaam"),
		@Mapping(target = "status", source = "status"),
		@Mapping(target = "beoordelingseenheid", source = "beoordelingsEenheid"),
		@Mapping(target = "rapportageDocumentId", source = "rapportageBijlage.id"),
		@Mapping(target = "vragenlijstDocumentId", source = "vragenlijstBijlage.id"),
	})
	MammaVisitatieDto mammaVisitatieToDto(MammaVisitatie mammaVisitatie);
}
