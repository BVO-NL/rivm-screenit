package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen;

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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_NAW_KEY;
import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY;
import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_OUD_KEY;
import static nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.ClientgegevensVerwijderenConstants.TOTAAL_AANTAL_CLIENTEN_REST_KEY;

@Component
@RequiredArgsConstructor
public class ClientgegevensVerwijderenListener extends BaseLogListener
{

	private final HibernateService hibernateService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CLIENTGEGEVENS_VERWIJDEREN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CLIENTGEGEVENS_VERWIJDEREN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();

		var aantalNaw = context.getLong(TOTAAL_AANTAL_CLIENTEN_NAW_KEY, 0);
		var aantalRest = context.getLong(TOTAAL_AANTAL_CLIENTEN_REST_KEY, 0);
		var aantalOud = context.getLong(TOTAAL_AANTAL_CLIENTEN_OUD_KEY, 0);
		var aantalPersoonsgegevensNietVerwijderd = context.getLong(TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY, 0);
		var aantalClientenNietVerwijderd = context.getLong(TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY, 0);

		var logEvent = super.eindLogging(jobExecution);
		var melding = "De gegevens van " + aantalNaw + " clienten verwijderd. " + aantalRest
			+ " clienten uit de extra beveiligde omgeving verwijderd.";

		if (aantalOud > 0)
		{
			melding += " Een poging gedaan om de oude dossiers van " + aantalOud + " clienten te legen.";
		}
		if (aantalPersoonsgegevensNietVerwijderd > 0)
		{
			melding += " Van " + aantalPersoonsgegevensNietVerwijderd + " clienten zijn de persoonsgegevens niet verwijderd omdat het dossier (nog) niet leeg is.";
		}
		if (aantalClientenNietVerwijderd > 0)
		{
			melding += aantalClientenNietVerwijderd + " clienten zijn niet verwijderd omdat de persoonsgegevens nog niet verwijderd zijn.";
		}
		logEvent.setMelding(melding);

		hibernateService.saveOrUpdate(logEvent);
		return logEvent;
	}
}
