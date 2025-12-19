package nl.rivm.screenit.dvabron.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Set;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dvabron.enums.DvaToetsResult;
import nl.rivm.screenit.dvabron.models.DvaToetsRequest;
import nl.rivm.screenit.dvabron.models.DvaToetsResponse;
import nl.rivm.screenit.dvabron.models.DvaToetsResultaat;
import nl.rivm.screenit.dvabron.services.DvaBronService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class DvaBronServiceImpl implements DvaBronService
{
	private final ClientService clientService;

	private final ClientDoelgroepService clientDoelgroepService;

	private static final Set<String> TOEGESTANE_MEDMIJ_CODES = Set.of("57", "51");

	public DvaToetsResponse checkBeschikbaarheidToets(DvaToetsRequest dvaToetsRequest)
	{
		var client = clientService.getClientByBsn(dvaToetsRequest.getBsn());
		return genereerResponse(client, dvaToetsRequest.getGegevens());
	}

	private DvaToetsResponse genereerResponse(Client client, String[] gegevens)
	{
		var toetsresultaat = controleerClient(client);

		var gemaakteToetsResultaten = maakToetsResultaten(gegevens, toetsresultaat);

		return genereerToetsResultaatResponse(gemaakteToetsResultaten);
	}

	private DvaToetsResult controleerClient(Client client)
	{
		if (client == null)
		{
			return DvaToetsResult.NO_INFORMATION_AVAILABLE;
		}

		if (clientDoelgroepService.totWelkeBevolkingsonderzoekenHoortDezeClient(client).isEmpty())
		{
			return DvaToetsResult.INVALID_AGE;
		}
		return DvaToetsResult.PERMITTED;
	}

	private static DvaToetsResultaat[] maakToetsResultaten(String[] gegevens, DvaToetsResult toetsresultaat)
	{
		var resultaten = new DvaToetsResultaat[gegevens.length];

		for (var i = 0; i < gegevens.length; i++)
		{
			var result = new DvaToetsResultaat();
			result.setMedmijcode(gegevens[i]);

			if (!isValideMedMijCode(gegevens[i]))
			{
				result.setToetsresultaat(DvaToetsResult.BLOCKED);
			}
			else
			{
				result.setToetsresultaat(toetsresultaat);
			}

			resultaten[i] = result;
		}
		return resultaten;
	}

	private static DvaToetsResponse genereerToetsResultaatResponse(DvaToetsResultaat[] toetsResultaten)
	{
		var response = new DvaToetsResponse();
		response.setResultaten(toetsResultaten);
		return response;
	}

	private static boolean isValideMedMijCode(String medmijCode)
	{
		return TOEGESTANE_MEDMIJ_CODES.contains(medmijCode);
	}
}
