package nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer;

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

@Component
@RequiredArgsConstructor
public class MedewerkerBeheerListener extends BaseLogListener
{
	public static final String TOTAAL_AANTAL_MAILS_KEY = "wachtwoordverlooptherinnering.totaal.aantal.mails";

	public static final String TOTAAL_AANTAL_MEDEWERKERS_GEINACTIVEERD = "medewerkerinactiveren.totaal.aantal.medewerkers";

	public static final String TOTAAL_AANTAL_ROLLEN_GEINACTIVEERD = "rolinactiveren.totaal.aantal.rollen";

	private final HibernateService hibernateService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MEDEWERKER_BEHEER_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MEDEWERKER_BEHEER_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();

		var totaalAantalMails = context.getLong(TOTAAL_AANTAL_MAILS_KEY, 0);
		var totaalAantalMedewerkers = context.getLong(TOTAAL_AANTAL_MEDEWERKERS_GEINACTIVEERD, 0);
		var totaalAantalRolKoppelingen = context.getLong(TOTAAL_AANTAL_ROLLEN_GEINACTIVEERD, 0);

		var logEvent = super.eindLogging(jobExecution);
		logEvent.setMelding(
			"Aantal mails verstuurd: " + totaalAantalMails + ". Aantal medewerkers ge&iuml;nactiveerd: " + totaalAantalMedewerkers + ". Aantal rol koppelingen ge&iuml;nactiveerd: "
				+ totaalAantalRolKoppelingen);

		hibernateService.saveOrUpdate(logEvent);
		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

}
