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

import nl.rivm.screenit.main.dto.algemeen.BezwaarMomentDto;
import nl.rivm.screenit.main.model.BriefActie;
import nl.rivm.screenit.main.service.algemeen.BezwaarService;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.BezwaarMoment;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { BezwaarMapper.class, BriefMapper.class })
public interface BezwaarMomentMapper
{
	@Mappings({
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "status", source = "status"),
		@Mapping(target = "bezwaarDatum", source = "bezwaarDatum"),
		@Mapping(target = "bezwaarBriefId", source = "bezwaarBrief.id"),
		@Mapping(target = "brieven", source = "brieven"),
		@Mapping(target = "bezwaren", source = "bezwaren"),
		@Mapping(target = "briefActies", source = "bezwaarMoment", qualifiedByName = "getBriefActies")

	})
	BezwaarMomentDto bezwaarMomentNaarDto(BezwaarMoment bezwaarMoment, @Context BezwaarService bezwaarService);

	@Named("getBriefActies")
	default List<BriefActie> getBriefActies(BezwaarMoment bezwaarMoment, @Context BezwaarService bezwaarService)
	{
		return bezwaarService.getBriefActies(bezwaarMoment);
	}
}
