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

import nl.rivm.screenit.main.dto.mamma.MammaUitstelDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaUitstel;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(config = ScreenitMapperConfig.class)
public interface MammaUitstelMapper
{
	@Mapping(target = "streefDatum", source = "uitstel.streefDatum")
	@Mapping(target = "standplaatsNaam", source = "uitstel.standplaats.naam")
	@Mapping(target = "plaats", source = "locatie.plaats")
	MammaUitstelDto uitstelNaarDto(MammaUitstel uitstel, MammaStandplaatsLocatie locatie);
}
