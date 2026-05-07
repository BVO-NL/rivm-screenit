package nl.rivm.screenit.mappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.uitnodiging.dto.OpenstaandeUitnodigingDto;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.service.BaseHoudbaarheidService;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.Date;
import java.util.Optional;

@Mapper(config = ScreenitMapperConfig.class, uses = { BaseHoudbaarheidService.class})
public interface OpenstaandeUitnodigingMapper
{
	@Mapping(target = "bevolkingsonderzoekType", constant = "CERVIX")
	@Mapping(target = "uitnodigingId", source = "uitnodigingsId")
	@Mapping(target = "ontvangstdatum", source = "monster.ontvangstdatum")
	@Mapping(target = "vervaldatum", source = "cervixUitnodiging", qualifiedByName = "bepaalVervalDatumCervix")
	OpenstaandeUitnodigingDto mapFromCervixUitnodiging(CervixUitnodiging cervixUitnodiging, @Context BaseHoudbaarheidService service);

	@Mapping(target = "bevolkingsonderzoekType", constant = "COLON")
	@Mapping(target = "uitnodigingId", source = "uitnodigingsId")
	@Mapping(target = "ontvangstdatum", source = "gekoppeldeFitRegistratie.afnameDatum")
	@Mapping(target = "vervaldatum", source = "colonUitnodiging", qualifiedByName = "bepaalVervalDatumColon")
	OpenstaandeUitnodigingDto mapFromColonUitnodiging(ColonUitnodiging colonUitnodiging, @Context BaseHoudbaarheidService service);

	@Named("bepaalVervalDatumCervix")
	default Date bepaalVervalDatumCervix(CervixUitnodiging cervixUitnodiging, @Context BaseHoudbaarheidService service)
	{
		return Optional.ofNullable(cervixUitnodiging.getMonster())
			.map(monster -> service.getZasHoudbaarheidVoor(monster.getMonsterId()))
			.map(cervixHoudbaarheid -> cervixHoudbaarheid.getVervalDatum())
			.orElse(null);
	}

	@Named("bepaalVervalDatumColon")
	default Date bepaalVervalDatumColon(ColonUitnodiging colonUitnodiging, @Context BaseHoudbaarheidService service)
	{
		return Optional.ofNullable(colonUitnodiging.getGekoppeldeFitRegistratie())
				.map(fit -> service.getFitHoudbaarheidVoor(fit.getBarcode()))
				.map(colonHoudbaarheid -> colonHoudbaarheid.getVervalDatum())
				.orElse(null);
	}
}
