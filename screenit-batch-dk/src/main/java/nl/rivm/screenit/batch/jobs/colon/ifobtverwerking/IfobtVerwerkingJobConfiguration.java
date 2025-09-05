package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking;

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
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.bestandopruimenstep.FITBestandOpruimenReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.bestandopruimenstep.FITBestandOpruimenWriter;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.uitslagopruimenstep.FITUitslagOpruimenReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.uitslagopruimenstep.FITUitslagOpruimenWriter;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingProcessor;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingWriter;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class IfobtVerwerkingJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job ifobtVerwerkingJob(IfobtVerwerkingListener listener, Step fitVerwerkingStep, Step fitUitslagOpruimenStep, Step fitBestandOpruimenStep)
	{
		return new JobBuilder(JobType.IFOBT_VERWERKING.name(), repository)
			.listener(listener)
			.start(fitVerwerkingStep)
			.next(fitUitslagOpruimenStep)
			.next(fitBestandOpruimenStep)
			.build();
	}

	@Bean
	public Step fitVerwerkingStep(IFOBTVerwerkingReader reader, IFOBTVerwerkingProcessor processor, IFOBTVerwerkingWriter writer)
	{
		return new StepBuilder("fitVerwerkingStep", repository)
			.<Long, IFOBTUitslag> chunk(10, transactionManager)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public Step fitUitslagOpruimenStep(FITUitslagOpruimenReader reader, FITUitslagOpruimenWriter writer)
	{
		return new StepBuilder("fitUitslagOpruimenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step fitBestandOpruimenStep(FITBestandOpruimenReader reader, FITBestandOpruimenWriter writer)
	{
		return new StepBuilder("fitBestandOpruimenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
