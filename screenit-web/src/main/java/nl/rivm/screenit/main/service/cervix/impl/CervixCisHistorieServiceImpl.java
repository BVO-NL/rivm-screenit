package nl.rivm.screenit.main.service.cervix.impl;

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

import java.util.ArrayList;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.cervix.CervixCisHistorieDto;
import nl.rivm.screenit.main.mappers.cervix.CervixCisHistorieMapper;
import nl.rivm.screenit.main.service.cervix.CervixCisHistorieService;
import nl.rivm.screenit.main.util.CervixCisHistoryUtil;
import nl.rivm.screenit.model.Client;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CervixCisHistorieServiceImpl implements CervixCisHistorieService
{
	private final CervixCisHistorieMapper cisHistorieMapper;

	@Override
	public CervixCisHistorieDto getCisHistorieByClient(Client client)
	{
		if (client.getCervixDossier() == null || client.getCervixDossier().getCisHistorie() == null)
		{
			return new CervixCisHistorieDto();
		}

		var regelsPerRonde = CervixCisHistoryUtil.getOngestructureerdeRegelsPerRonde(
			client.getCervixDossier().getCisHistorie(), false);
		var cisHistorieDto = new CervixCisHistorieDto();

		for (var ronde : regelsPerRonde.keySet())
		{
			var historieVoorRonde = cisHistorieDto.getRondes().computeIfAbsent(ronde, k -> new ArrayList<>());
			historieVoorRonde.addAll(regelsPerRonde.get(ronde).stream().map(cisHistorieMapper::ongestructureerdeRegelToDto).toList());
		}
		return cisHistorieDto;
	}
}
