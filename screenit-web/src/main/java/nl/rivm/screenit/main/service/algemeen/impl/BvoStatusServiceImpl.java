package nl.rivm.screenit.main.service.algemeen.impl;

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

import java.util.Arrays;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.BvoStatusDto;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.algemeen.BvoStatusService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class BvoStatusServiceImpl implements BvoStatusService
{
	private final ColonDossierBaseService colonDossierBaseService;

	private final ClientService clientService;

	private final DossierService dossierService;

	public List<BvoStatusDto> getBvoStatus(Client client)
	{
		return Arrays.stream(Bevolkingsonderzoek.values()).filter(bvo -> clientService.getDossier(client, bvo) != null).map(bvo ->
		{
			var dto = new BvoStatusDto();
			dto.setBevolkingsonderzoek(bvo);

			var dossier = clientService.getDossier(client, bvo);
			dto.setActief(dossierService.isDossierActief(dossier));

			if (bvo == Bevolkingsonderzoek.COLON)
			{
				dto.setDatumVolgendeUitnodiging(colonDossierBaseService.getDatumVolgendeUitnodiging(client.getColonDossier()));
			}

			return dto;
		}).toList();
	}
}
