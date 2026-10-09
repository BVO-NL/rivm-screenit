package nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie;

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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.step.CervixCambiosInclusieDecider;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.step.CervixCambiosInclusieReader;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.step.CervixCambiosInclusieWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.job.Job;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.Step;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixCambiosInclusieJobConfiguration extends AbstractJobConfiguration
{
	@Bean
	public Job cambiosInclusieJob(CervixCambiosInclusieJobListener listener, Step dummyStep, CervixCambiosInclusieDecider cambiosInclusieDecider,
		Step cambiosInclusieStep)
	{
		return new JobBuilder(JobType.CERVIX_CAMBIOS_INCLUSIE.name(), repository)
			.listener(listener)
			.start(dummyStep)
			.next(cambiosInclusieDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(cambiosInclusieStep)
			.from(cambiosInclusieDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).end()
			.end().build();
	}

	@Bean
	public Step cambiosInclusieStep(CervixCambiosInclusieReader reader, CervixCambiosInclusieWriter writer)
	{
		return new StepBuilder("cambiosInclusieStep", repository)
			.<Long, Long> chunk(1, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
