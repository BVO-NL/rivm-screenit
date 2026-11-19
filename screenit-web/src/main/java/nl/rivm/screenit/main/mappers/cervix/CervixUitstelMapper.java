package nl.rivm.screenit.main.mappers.cervix;

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

import java.time.LocalDate;

import nl.rivm.screenit.main.dto.algemeen.CervixUitstelDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.util.DateUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface CervixUitstelMapper
{
	@Mapping(target = "uitstelType", source = "ronde", qualifiedByName = "lopendUitstelType")
	@Mapping(target = "uitstellenTotDatum", source = "ronde", qualifiedByName = "lopendUitstellenTotDatum")
	@Mapping(target = "zasAanvraagWordtGeannuleerd", source = "ronde.laatsteZasUitnodiging", qualifiedByName = "isOnverstuurdeZasAanvraag")
	CervixUitstelDto naarDto(CervixScreeningRonde ronde);

	@Named("lopendUitstelType")
	default CervixUitstelType getLopendUitstelType(CervixScreeningRonde ronde)
	{
		var uitstel = ronde.getUitstel();
		return uitstel != null && uitstel.getGeannuleerdDatum() == null ? uitstel.getUitstelType() : null;
	}

	@Named("lopendUitstellenTotDatum")
	default LocalDate getLopendUitstellenTotDatum(CervixScreeningRonde ronde)
	{
		var uitstel = ronde.getUitstel();
		return uitstel != null && uitstel.getGeannuleerdDatum() == null ? DateUtil.toLocalDate(uitstel.getUitstellenTotDatum()) : null;
	}

	@Named("isOnverstuurdeZasAanvraag")
	default boolean isOnverstuurdeZasAanvraag(CervixUitnodiging zasUitnodiging)
	{
		return zasUitnodiging != null && zasUitnodiging.getVerstuurdDatum() == null && zasUitnodiging.getGeannuleerdDatum() == null;
	}
}
