package nl.rivm.screenit.batch.jobs.cervix.order;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.CervixBaseLogListener;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixOrderListener extends CervixBaseLogListener
{
	private final SimplePreferenceService preferenceService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);

		var startdatumAanleveringGenotyperingString = preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());
		getJobExecution().getExecutionContext().putString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name(), startdatumAanleveringGenotyperingString);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_ORDER_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_ORDER_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var event = super.eindLogging(jobExecution);

		var context = jobExecution.getExecutionContext();
		long aangemaakt = context.getLong(CervixOrderConstants.KEY_ORDER_AANGEMAAKT, 0);

		long verstuurd = context.getLong(CervixOrderConstants.KEY_ORDER_VERSTUURD, 0);
		if (StringUtils.isBlank(event.getMelding()))
		{
			String melding = "Er zijn " + aangemaakt + " order bericht(en) aangemaakt en " + verstuurd + " order bericht(en) verstuurd";
			event.setMelding(melding);
		}
		return event;
	}
}
