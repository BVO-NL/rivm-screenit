package nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.colon.ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonFitAnalyseResultaatSetVerwerkingRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonFitAnalyseResultaatSetVerwerkingListener extends BaseLogListener
{

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var verwerkingLogEvent = new ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent();
		var rapportage = new ColonFitAnalyseResultaatSetVerwerkingRapportage();
		rapportage.setDatumVerwerking(currentDateSupplier.getDate());
		verwerkingLogEvent.setRapportage(rapportage);
		jobExecution.getExecutionContext().put(ColonFitAnalyseResultaatSetVerwerkingConstants.RAPPORTAGEKEYVERWERKING, verwerkingLogEvent);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_VERWERKING_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_VERWERKING_AFGEROND;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		String key = ColonFitAnalyseResultaatSetVerwerkingConstants.RAPPORTAGEKEYVERWERKING;
		var executionContext = getJobExecution().getExecutionContext();
		if (executionContext.containsKey(key))
		{
			return (ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent) executionContext.get(key);
		}
		return null;
	}
}
