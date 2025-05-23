package nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen;

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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.cleanupstep.ColonUitnodigingenBrievenCleanUpReader;
import nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.cleanupstep.ColonUitnodigingenBrievenCleanUpWriter;
import nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen.versturenstep.ColonUitnodigingenVersturenTasklet;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class UitnodigingenVersturenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job uitnodigingVersturenNaarInpakcentrumJob(UitnodigingenVersturenListener listener, Step uitnodigingenBrievenCleanupStep, Step uitnodigingenVersturenStep)
	{
		return new JobBuilder(JobType.UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM_JOB_DK.name(), repository)
			.listener(listener)
			.start(uitnodigingenBrievenCleanupStep)
			.next(uitnodigingenVersturenStep)
			.build();
	}

	@Bean
	public Step uitnodigingenBrievenCleanupStep(ColonUitnodigingenBrievenCleanUpReader reader, ColonUitnodigingenBrievenCleanUpWriter writer)
	{
		return new StepBuilder("uitnodigingenBrievenCleanupStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uitnodigingenVersturenStep(ColonUitnodigingenVersturenTasklet tasklet)
	{
		return new StepBuilder("uitnodigingenVersturenStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

}
