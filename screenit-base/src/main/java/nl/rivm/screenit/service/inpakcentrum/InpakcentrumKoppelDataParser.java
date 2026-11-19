package nl.rivm.screenit.service.inpakcentrum;

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

import java.io.IOException;
import java.util.Base64;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.exceptions.OngeldigeKoppelDataException;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.util.ScreenitObjectMapper;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.type.TypeReference;

@Component
@RequiredArgsConstructor
public class InpakcentrumKoppelDataParser
{
	private final ScreenitObjectMapper objectMapper;

	public List<InpakcentrumKoppelDataDto> leesKoppelData(String base64KoppelData) throws OngeldigeKoppelDataException
	{
		if (StringUtils.isBlank(base64KoppelData))
		{
			throw new OngeldigeKoppelDataException("Koppeldata is leeg");
		}
		List<InpakcentrumKoppelDataDto> koppelData;
		try
		{
			var json = Base64.getDecoder().decode(base64KoppelData);
			koppelData = objectMapper.readValue(json, new TypeReference<>()
			{
			});
		}
		catch (IllegalArgumentException | IOException e)
		{
			throw new OngeldigeKoppelDataException("Koppeldata is geen geldige Base64 gecodeerde JSON", e);
		}
		if (koppelData == null || koppelData.contains(null))
		{
			throw new OngeldigeKoppelDataException("Koppeldata bevat geen lijst of bevat lege elementen");
		}
		return koppelData;
	}
}
