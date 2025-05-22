package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.persoonsgegevensverwijderen;

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

import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_NAW_KEY;
import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_PERSOONSGEGEVENS_CLIENTEN_NIET_VERWIJDERD_KEY;

@Slf4j
@Component
@AllArgsConstructor
public class PersoonsgegevensVerwijderenWriter extends BaseWriter<Client>
{
	private final BezwaarService bezwaarService;

	@Override
	protected void write(Client client) throws Exception
	{
		if (zijnDossiersLeeg(client))
		{
			try
			{
				bezwaarService.verwijderPersoonsgegevens(client);
				LOG.info("Persoonsgegevens van client '{}' verwijderd", client.getId());
				aantalContextOphogen(TOTAAL_AANTAL_CLIENTEN_NAW_KEY);
			}
			catch (Exception ex)
			{
				LOG.error("Fout bij verwijderen persoonsgegevens van client '{}'", client.getId(), ex);
			}
		}
		else
		{
			LOG.info("Client '{}' heeft nog gevulde dossiers, persoonsgegevens niet verwijderd", client.getId());
			aantalContextOphogen(TOTAAL_AANTAL_PERSOONSGEGEVENS_CLIENTEN_NIET_VERWIJDERD_KEY);
		}
	}

	private boolean zijnDossiersLeeg(Client client)
	{
		if (client.getCervixDossier() != null && (client.getCervixDossier().getLaatsteScreeningRonde() != null || client.getCervixDossier().getLaatsteAfmelding() != null))
		{
			return false;
		}
		if (client.getColonDossier() != null && (client.getColonDossier().getLaatsteScreeningRonde() != null || client.getColonDossier().getLaatsteAfmelding() != null))
		{
			return false;
		}
		if (client.getMammaDossier() != null && (client.getMammaDossier().getLaatsteScreeningRonde() != null || client.getMammaDossier().getLaatsteAfmelding() != null))
		{
			return false;
		}
		return true;
	}
}
