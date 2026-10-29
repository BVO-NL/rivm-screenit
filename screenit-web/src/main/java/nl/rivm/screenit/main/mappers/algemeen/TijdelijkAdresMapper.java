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

import nl.rivm.screenit.main.dto.algemeen.TijdelijkAdresDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TijdelijkAdres;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class)
public interface TijdelijkAdresMapper
{
	@Mappings({
		@Mapping(source = "startDatum", target = "begindatum"),
		@Mapping(source = "eindDatum", target = "einddatum"),
		@Mapping(source = "straat", target = "straat"),
		@Mapping(source = "huisnummerAanduiding", target = "huisnummerAanduiding"),
		@Mapping(target = "clientId", ignore = true)
	})
	TijdelijkAdresDto tijdelijkAdresToDto(TijdelijkAdres tijdelijkAdres);

	@Mappings({
		@Mapping(source = "begindatum", target = "startDatum"),
		@Mapping(source = "einddatum", target = "eindDatum"),
		@Mapping(source = "straat", target = "straat"),
		@Mapping(source = "huisnummerAanduiding", target = "huisnummerAanduiding"),
		@Mapping(target = "id", ignore = true),
		@Mapping(target = "soort", ignore = true),
		@Mapping(target = "geheimadres", ignore = true),
		@Mapping(target = "tijdelijk", ignore = true),
		@Mapping(target = "gemeente", ignore = true),
		@Mapping(target = "gemeentedeel", ignore = true),
		@Mapping(target = "land", ignore = true),
		@Mapping(target = "aanschrijfAdres", ignore = true),
		@Mapping(target = "gemeenteCode", ignore = true),
		@Mapping(target = "locatieBeschrijving", ignore = true),
		@Mapping(target = "postcodeCoordinaten", ignore = true)
	})
	TijdelijkAdres dtoToTijdelijkAdres(TijdelijkAdresDto dto);

	@Mappings({
		@Mapping(source = "begindatum", target = "startDatum"),
		@Mapping(source = "einddatum", target = "eindDatum"),
		@Mapping(source = "straat", target = "straat"),
		@Mapping(source = "huisnummerAanduiding", target = "huisnummerAanduiding"),
		@Mapping(target = "id", ignore = true),
		@Mapping(target = "soort", ignore = true),
		@Mapping(target = "geheimadres", ignore = true),
		@Mapping(target = "tijdelijk", ignore = true),
		@Mapping(target = "gemeente", ignore = true),
		@Mapping(target = "gemeentedeel", ignore = true),
		@Mapping(target = "land", ignore = true),
		@Mapping(target = "aanschrijfAdres", ignore = true),
		@Mapping(target = "gemeenteCode", ignore = true),
		@Mapping(target = "locatieBeschrijving", ignore = true),
		@Mapping(target = "postcodeCoordinaten", ignore = true)
	})
	void updateTijdelijkAdres(@MappingTarget TijdelijkAdres target, TijdelijkAdresDto dto);

	default TijdelijkAdres naarTijdelijkAdres(Client client, TijdelijkAdresDto dto)
	{
		var huidigTijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (huidigTijdelijkAdres == null)
		{
			return dtoToTijdelijkAdres(dto);
		}
		updateTijdelijkAdres(huidigTijdelijkAdres, dto);
		return huidigTijdelijkAdres;
	}
}
