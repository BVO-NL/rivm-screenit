package nl.rivm.screenit.clientportaal.mappers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import nl.rivm.screenit.clientportaal.mappers.AdresMapper;
import nl.rivm.screenit.clientportaal.model.colon.ColonIntakeafspraakDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = { AdresMapper.class })
public interface ColonIntakeAfspraakMapper
{
	@Mappings({
		@Mapping(target = "adres", source = "kamer.intakelocatie.adres"),
		@Mapping(target = "vanaf", source = "vanaf"),
		@Mapping(target = "afspraakAfgezegd", source = "afspraak", qualifiedByName = "isAfspraakAfgezegd"),
		@Mapping(target = "andereIntakelocatieOpVerzoekClient", source = "afspraak", qualifiedByName = "bepaalAndereIntakeLocatieOpVerzoekClient"),
		@Mapping(target = "naamIntakelocatie", source = "kamer.intakelocatie.naam"),
		@Mapping(target = "type", source = "intakeafspraakType")
	})
	ColonIntakeafspraakDto intakeAfspraakNaarDto(ColonIntakeAfspraak afspraak);

	@Named("bepaalAndereIntakeLocatieOpVerzoekClient")
	default boolean bepaalAndereIntakeLocatieOpVerzoekClient(ColonIntakeAfspraak afspraak)
	{
		var conclusie = afspraak.getConclusie();
		return afspraak.getStatus() == ColonAfspraakStatus.UITGEVOERD && conclusie != null
			&& conclusie.getType() == ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE;
	}

	@Named("isAfspraakAfgezegd")
	default boolean isAfspraakAfgezegd(ColonIntakeAfspraak afspraak)
	{
		return afspraak.getAfgezegdOp() != null;
	}
}
