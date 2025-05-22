package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.clientgegevensverwijderen;

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
import nl.rivm.screenit.service.BezwaarService;

import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY;
import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_REST_KEY;

@Slf4j
@Component
@AllArgsConstructor
public class ClientgegevensVerwijderenWriter extends BaseWriter<Client>
{
	private final BezwaarService bezwaarService;

	@Override
	protected void write(Client client) throws Exception
	{
		if (zijnPersoonsgegevensVerwijderd(client))
		{
			try
			{
				bezwaarService.verwijderClient(client);
				LOG.info("Gegevens van client '{}' verwijderd", client.getId());

				aantalContextOphogen(TOTAAL_AANTAL_CLIENTEN_REST_KEY);
			}
			catch (Exception ex)
			{
				LOG.error("Fout bij verwijderen gegevens van client '{}'", client.getId(), ex);
			}
		}
		else
		{
			LOG.info("Client '{}' heeft nog dossiers, persoonsgegevens niet verwijderd", client.getId());
			aantalContextOphogen(TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY);
		}
	}

	private boolean zijnPersoonsgegevensVerwijderd(Client client)
	{
		return client.getColonDossier() == null
			&& client.getCervixDossier() == null
			&& client.getMammaDossier() == null
			&& client.getBezwaarMomenten().size() < 2
			&& client.getAfspraken().isEmpty()
			&& client.getAlgemeneBrieven().isEmpty()
			&& client.getContacten().isEmpty()
			&& client.getGbaVragen().isEmpty()
			&& client.getDocuments().isEmpty();
	}
}
