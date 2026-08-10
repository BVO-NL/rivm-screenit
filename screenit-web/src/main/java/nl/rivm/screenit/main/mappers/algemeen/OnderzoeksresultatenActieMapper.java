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

import nl.rivm.screenit.main.dto.algemeen.OnderzoeksresultatenActieDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.OnderzoeksresultatenActieType;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { BriefMapper.class })
public interface OnderzoeksresultatenActieMapper
{
	@Mappings({
		@Mapping(target = "id", source = "id"),
		@Mapping(target = "moment", source = "moment"),
		@Mapping(target = "type", source = "type"),
		@Mapping(target = "getekendeBriefId", source = "getekendeBrief.id"),
		@Mapping(target = "bevolkingsonderzoek", source = "actie", qualifiedByName = "bevolkingsonderzoek"),
		@Mapping(target = "brieven", source = "brieven")
	})
	OnderzoeksresultatenActieDto onderzoeksresultatenActieNaarDto(OnderzoeksresultatenActie actie);

	@Named("bevolkingsonderzoek")
	default Bevolkingsonderzoek actieNaarBevolkingsonderzoek(OnderzoeksresultatenActie actie)
	{
		return switch (actie.getType())
		{
			case OnderzoeksresultatenActieType.ONDERZOEKSRESULTATEN_VERWIJDERD_DK -> Bevolkingsonderzoek.COLON;
			case OnderzoeksresultatenActieType.ONDERZOEKSRESULTATEN_VERWIJDERD_BK -> Bevolkingsonderzoek.MAMMA;
			case OnderzoeksresultatenActieType.ONDERZOEKSRESULTATEN_VERWIJDERD_BMHK -> Bevolkingsonderzoek.CERVIX;
		};
	}
}
