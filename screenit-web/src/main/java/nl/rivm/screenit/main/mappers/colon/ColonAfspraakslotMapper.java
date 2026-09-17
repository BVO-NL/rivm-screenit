package nl.rivm.screenit.main.mappers.colon;

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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.main.dto.colon.ColonAfspraakslotDto;
import nl.rivm.screenit.main.dto.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.util.DateUtil;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { ColonIntakelocatieMapper.class })
public interface ColonAfspraakslotMapper
{
	@Named("dateToLocalDateTime")
	default LocalDateTime conversionDatum(Date datum)
	{
		return DateUtil.toLocalDateTime(datum);
	}

	@Mappings({
		@Mapping(source = "afspraakslotId", target = "id"),
		@Mapping(source = "startDatum", target = "vanaf", qualifiedByName = "dateToLocalDateTime"),
		@Mapping(source = "eindDatum", target = "tot", qualifiedByName = "dateToLocalDateTime"),
		@Mapping(source = "kamer", target = "kamer"),
		@Mapping(source = "capaciteitMeeBepaald", target = "capaciteitMeeBepaald"),
		@Mapping(source = "kamerId", target = "kamerId"),
		@Mapping(target = "aantalBlokken", ignore = true),
		@Mapping(target = "alleenValidatie", ignore = true),
		@Mapping(target = "herhaling", ignore = true),
	})
	ColonAfspraakslotDto roosterListItemViewWrapperToColonAfspraakslotDto(ColonAfspraakslotListViewWrapper wrapper);

	@Mappings({
		@Mapping(source = "startTijd", target = "startTijd", qualifiedByName = "dateToLocalDateTime"),
		@Mapping(source = "eindTijd", target = "eindTijd", qualifiedByName = "dateToLocalDateTime"),
		@Mapping(source = "intakelocatieId", target = "intakelocatie", qualifiedByName = "getIntakelocatie"),
		@Mapping(source = "plaats", target = "plaats"),
		@Mapping(source = "afstand", target = "afstand"),
	})
	ColonVrijSlotZonderKamerDto vrijSlotZonderKamerNaarDto(VrijSlotZonderKamer vrijSlotZonderKamer, @Context ColonIntakelocatieService intakelocatieService);

	@Named("getIntakelocatie")
	default ColonIntakelocatie getIntakelocatie(Long intakelocatieId, @Context ColonIntakelocatieService intakelocatieService)
	{
		return intakelocatieService.getIntakelocatieById(intakelocatieId);
	}
}
