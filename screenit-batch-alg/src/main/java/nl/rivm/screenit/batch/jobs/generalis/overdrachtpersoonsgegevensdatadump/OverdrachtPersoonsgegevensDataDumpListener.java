package nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.HibernateService;

import org.springframework.batch.core.job.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class OverdrachtPersoonsgegevensDataDumpListener extends BaseLogListener
{
	public static final String TOTAAL_AANTAL_GEGENEREERD_KEY = "overdrachtpersoonsgegevensdatadump.totaal.aantal.gegenereerd";

	public static final String TOTAAL_AANTAL_OPGERUIMD_KEY = "overdrachtpersoonsgegevensdatadump.totaal.aantal.opgeruimd";

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.OVERDRACHT_PERSOONSGEGEVENS_DATADUMP_BATCH_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.OVERDRACHT_PERSOONSGEGEVENS_DATADUMP_BATCH_AFGEROND;
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

		var totaalAantalGegenereerd = context.getLong(TOTAAL_AANTAL_GEGENEREERD_KEY, 0);
		var totaalAantalOpgeruimd = context.getLong(TOTAAL_AANTAL_OPGERUIMD_KEY, 0);

		var logEvent = super.eindLogging(jobExecution);
		logEvent.setMelding("Er zijn " + totaalAantalGegenereerd + " data dumps gegenereerd en " + totaalAantalOpgeruimd + " data dumps opgeruimd");

		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

}
