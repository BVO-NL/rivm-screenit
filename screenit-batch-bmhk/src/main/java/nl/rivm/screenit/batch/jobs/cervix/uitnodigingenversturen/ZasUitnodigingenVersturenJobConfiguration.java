package nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen;

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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.cleanupstep.ZasUitnodigingenBrievenCleanUpReader;
import nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.cleanupstep.ZasUitnodigingenBrievenCleanUpWriter;
import nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen.versturenstep.ZasUitnodigingenVersturenTasklet;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ZasUitnodigingenVersturenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job zasUitnodigingenVerstuenNaarInpakcentrumJob(ZasUitnodigingenVersturenListener listener, Step zasUitnodigingenBrievenCleanupStep, Step uitnodigingenVersturenStep)
	{
		return new JobBuilder(JobType.CERVIX_ZAS_UITNODIGING_VERSTUREN_NAAR_INPAKCENTRUM.name(), repository)
			.listener(listener)
			.start(zasUitnodigingenBrievenCleanupStep)
			.next(uitnodigingenVersturenStep)
			.build();
	}

	@Bean
	public Step zasUitnodigingenBrievenCleanupStep(ZasUitnodigingenBrievenCleanUpReader reader, ZasUitnodigingenBrievenCleanUpWriter writer)
	{
		return new StepBuilder("zasUitnodigingenBrievenCleanupStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uitnodigingenVersturenStep(ZasUitnodigingenVersturenTasklet tasklet)
	{
		return new StepBuilder("uitnodigingenVersturenStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}
}
