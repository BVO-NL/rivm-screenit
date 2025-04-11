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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep.CervixOrderAanmaakReader;
import nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep.CervixOrderAanmaakWriter;
import nl.rivm.screenit.batch.jobs.cervix.order.versturenstep.CervixOrderVersturenReader;
import nl.rivm.screenit.batch.jobs.cervix.order.versturenstep.CervixOrderVersturenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixOrderJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job orderJob(CervixOrderListener listener, Step orderAanmaakStep, Step orderVersturenStep)
	{
		return new JobBuilder(JobType.CERVIX_ORDER.name(), repository)
			.listener(listener)
			.start(orderAanmaakStep)
			.next(orderVersturenStep)
			.build();
	}

	@Bean
	public Step orderAanmaakStep(CervixOrderAanmaakReader reader, CervixOrderAanmaakWriter writer)
	{
		return new StepBuilder("orderAanmaakStep", repository)
			.<Long, Long> chunk(10, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step orderVersturenStep(CervixOrderVersturenReader reader, CervixOrderVersturenWriter writer)
	{
		return new StepBuilder("orderVersturenStep", repository)
			.<Long, Long> chunk(1, transactionManager) 
			.reader(reader)
			.writer(writer)
			.build();
	}

}
