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

import nl.rivm.screenit.main.dto.algemeen.AfspraakDto;
import nl.rivm.screenit.main.dto.algemeen.AfspraakLocatieDto;
import nl.rivm.screenit.main.mappers.algemeen.AdresMapper;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;

import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class, uses = { AdresMapper.class })
public interface MammaAfspraakMapper
{
	@Mappings({
		@Mapping(target = "id", source = "afspraak.id"),
		@Mapping(target = "vanaf", source = "afspraak.vanaf"),
		@Mapping(target = "locatie", ignore = true),
		@Mapping(target = "locatieBeschrijving", source = "locatie.locatieBeschrijving"),
		@Mapping(target = "digitaal", constant = "false")
	})
	AfspraakDto afspraakNaarDto(MammaAfspraak afspraak, MammaStandplaatsLocatie locatie);

	@Mapping(target = "naam", source = "standplaats.naam")
	@Mapping(target = "adres", source = "locatie")
	AfspraakLocatieDto afspraakLocatieNaarDto(MammaStandplaats standplaats, MammaStandplaatsLocatie locatie);

	@AfterMapping
	default void voegLocatieToe(MammaAfspraak afspraak, MammaStandplaatsLocatie locatie, @MappingTarget AfspraakDto afspraakDto)
	{
		var standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
		afspraakDto.setLocatie(afspraakLocatieNaarDto(standplaats, locatie));
	}
}
