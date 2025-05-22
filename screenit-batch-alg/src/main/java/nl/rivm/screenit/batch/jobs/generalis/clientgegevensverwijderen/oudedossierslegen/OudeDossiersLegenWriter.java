package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.oudedossierslegen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.BerichtToBatchService;

import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_OUD_KEY;

@Slf4j
@Component
@AllArgsConstructor
public class OudeDossiersLegenWriter extends BaseWriter<Client>
{
	private final BerichtToBatchService verwerkBerichtService;

	@Override
	protected void write(Client client) throws Exception
	{
		var clientId = client.getId();
		verwerkBerichtService.queueDossierLegenBericht(clientId, Bevolkingsonderzoek.CERVIX);
		verwerkBerichtService.queueDossierLegenBericht(clientId, Bevolkingsonderzoek.COLON);
		verwerkBerichtService.queueDossierLegenBericht(clientId, Bevolkingsonderzoek.MAMMA);
		LOG.info("Opdracht voor legen van (oude) dossier(s) van client '{}' verstuurd naar batch applicaties", clientId);

		aantalContextOphogen(TOTAAL_AANTAL_CLIENTEN_OUD_KEY);
	}
}
