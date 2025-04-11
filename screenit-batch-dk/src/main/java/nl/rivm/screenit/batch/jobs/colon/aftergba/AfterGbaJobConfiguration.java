package nl.rivm.screenit.batch.jobs.colon.aftergba;

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
import nl.rivm.screenit.batch.jobs.colon.aftergba.overledenstep.OverledenReader;
import nl.rivm.screenit.batch.jobs.colon.aftergba.overledenstep.OverledenWriter;
import nl.rivm.screenit.batch.jobs.colon.aftergba.retourzendingstep.RetourzendingReader;
import nl.rivm.screenit.batch.jobs.colon.aftergba.retourzendingstep.RetourzendingWriter;
import nl.rivm.screenit.batch.jobs.colon.aftergba.uitnodigingsgebiedstep.UitnodigingsgebiedReader;
import nl.rivm.screenit.batch.jobs.colon.aftergba.uitnodigingsgebiedstep.UitnodigingsgebiedWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AfterGbaJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job afterGbaJob(AfterGbaListener listener, Step retourzendingStep, Step uitnodigingsgebiedStep, Step overledenStep)
	{
		return new JobBuilder(JobType.COLON_NA_GBA.name(), repository)
			.listener(listener)
			.start(retourzendingStep)
			.next(uitnodigingsgebiedStep)
			.next(overledenStep)
			.build();
	}

	@Bean
	public Step retourzendingStep(RetourzendingReader reader, RetourzendingWriter writer)
	{
		return new StepBuilder("retourzendingStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uitnodigingsgebiedStep(UitnodigingsgebiedReader reader, UitnodigingsgebiedWriter writer)
	{
		return new StepBuilder("uitnodigingsgebiedStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step overledenStep(OverledenReader reader, OverledenWriter writer)
	{
		return new StepBuilder("overledenStep", repository)
			.<Long, Long> chunk(20, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
