package nl.rivm.screenit.batch.jobs.generalis.gba.indicatieterugzettenvoordoelgroepstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class IndicatieTerugzettenVoorDoelgroepWriter extends BaseWriter<Client>
{
	@Autowired
	private BaseGbaVraagService gbaVraagService;

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Override
	protected void write(Client client) throws Exception
	{
		client.setRedenIntrekkenGbaIndicatieDoorBvo(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN);
		client.setGbaStatus(GbaStatus.INDICATIE_AANGEVRAAGD);
		logService.logGebeurtenis(LogGebeurtenis.CLIENT_BRP_INDICATIE_AANGEVRAAGD, clientService.getScreeningOrganisatieVan(client), client,
			RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN.name());
		gbaVraagService.verzoekPlaatsIndicatie(null, client, RedenGbaVraag.BRIEF_VERSTUREN, null);
	}
}
