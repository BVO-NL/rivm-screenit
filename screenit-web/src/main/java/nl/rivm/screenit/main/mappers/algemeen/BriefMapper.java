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

import nl.rivm.screenit.main.dto.algemeen.BriefDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.util.BriefUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface BriefMapper
{
	@Mappings({
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "briefType", source = "briefType"),
		@Mapping(target = "documentNaam", source = "brief", qualifiedByName = "documentNaam"),
		@Mapping(target = "verstuurdVoorAfdrukkenOp", source = "verstuurdVoorAfdrukkenOp"),
		@Mapping(target = "tegengehouden", source = "brief", qualifiedByName = "tegengehouden"),
		@Mapping(target = "vervangen", source = "vervangen"),
		@Mapping(target = "gegenereerd", source = "gegenereerd"),
		@Mapping(target = "herdrukBrief", source = "brief", qualifiedByName = "herdrukBrief"),
		@Mapping(target = "creatieDatum", source = "creatieDatum"),
	})
	BriefDto briefNaarDto(Brief brief);

	@Named("documentNaam")
	default String getNaamVanDocument(Brief brief)
	{
		return brief.isGegenereerd() ? brief.getTemplateNaam() : brief.getBriefType().getWeergaveNaam();
	}

	@Named("tegengehouden")
	default boolean tegengehouden(Brief brief)
	{
		return BriefUtil.isTegengehouden(brief);
	}

	@Named("herdrukBrief")
	default Brief herdrukBrief(Brief brief)
	{
		if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
		{
			return BriefUtil.getHerdruk(clientBrief);
		}
		return null;
	}
}
