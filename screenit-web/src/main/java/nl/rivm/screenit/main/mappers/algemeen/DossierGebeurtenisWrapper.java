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

import nl.rivm.screenit.main.dto.algemeen.BezwaarDossierGebeurtenisDto;
import nl.rivm.screenit.main.model.BezwaarDossierGebeurtenis;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { BriefMapper.class })
public interface DossierGebeurtenisWrapper
{
	@Mappings({
		@Mapping(target = "brieven", source = "bezwaarModel.object.brieven"),
		@Mapping(target = "documentId", source = "gebeurtenis", qualifiedByName = "getDocumentId"),
		@Mapping(target = "bezwaarMomentId", source = "bezwaarModel.object.id"),
		@Mapping(target = "bron", source = "bron"),
		@Mapping(target = "gebeurtenis", source = "dossierGebeurtenisType"),
		@Mapping(target = "datum", source = "tijd"),
		@Mapping(target = "status", source = "omschrijving")
	})
	BezwaarDossierGebeurtenisDto bezwaarDossierGebeurtenisNaarDto(BezwaarDossierGebeurtenis gebeurtenis);

	@Named("getDocumentId")
	default Long getDocumentId(BezwaarDossierGebeurtenis gebeurtenis)
	{
		var bezwaarBrief = gebeurtenis.getBezwaarModel().getObject().getBezwaarBrief();
		return bezwaarBrief == null ? null : bezwaarBrief.getId();
	}
}
