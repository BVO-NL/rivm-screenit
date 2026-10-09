package nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.AbstractCervixProjectGroepWriter;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.CervixCambiosInclusieConstants;

import org.springframework.batch.core.job.JobExecution;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.job.flow.JobExecutionDecider;
import org.springframework.batch.core.step.StepExecution;
import org.springframework.stereotype.Component;

@Component
public class CervixCambiosInclusieDecider implements JobExecutionDecider
{
	@Override
	public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution)
	{
		var context = jobExecution.getExecutionContext();
		if (context.containsKey(AbstractCervixProjectGroepWriter.PROJECT_ID) && context.containsKey(CervixCambiosInclusieConstants.LABORATORIUM_IDS))
		{
			return FlowExecutionStatus.FAILED;
		}
		else
		{
			return FlowExecutionStatus.COMPLETED;
		}
	}
}
