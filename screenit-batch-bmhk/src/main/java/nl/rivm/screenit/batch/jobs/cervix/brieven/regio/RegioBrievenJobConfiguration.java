package nl.rivm.screenit.batch.jobs.cervix.regioBrieven.regio;

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
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.RegioBrievenListener;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.cleanupstep.RegioBrievenCleanUpReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.cleanupstep.RegioBrievenCleanUpWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.controlestep.RegioBrievenControleReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.controlestep.RegioBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep.RegioBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep.RegioBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep.RegioBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep.RegioBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep.LabformulierGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep.LabformulierGenererenReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep.LabformulierGenererenWriter;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class RegioBrievenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job regioBrievenJob(RegioBrievenListener listener, Step regioBrievenCleanupStep, Step regioBrievenGenererenPartitionerStep, Step labformulierenGenererenPartitionerStep,
		Step regioBrievenControleStep)
	{
		return new JobBuilder(JobType.REGIO_BRIEVEN.name(), repository)
			.listener(listener)
			.start(regioBrievenCleanupStep)
			.next(regioBrievenGenererenPartitionerStep)
			.next(labformulierenGenererenPartitionerStep)
			.next(regioBrievenControleStep)
			.build();
	}

	@Bean
	public Step regioBrievenCleanupStep(RegioBrievenCleanUpReader reader, RegioBrievenCleanUpWriter writer)
	{
		return new StepBuilder("regioBrievenCleanupStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step regioBrievenGenererenPartitionerStep(RegioBrievenGenererenPartitioner partitioner, TaskExecutorPartitionHandler regioBrievenPartitionHandler,
		Step regioBrievenGenererenStep)
	{
		return new StepBuilder("regioBrievenGenererenPartitionerStep", repository)
			.partitioner("regioBrievenGenererenStep", partitioner)
			.partitionHandler(regioBrievenPartitionHandler)
			.step(regioBrievenGenererenStep)
			.build();
	}

	@Bean
	public Step labformulierenGenererenPartitionerStep(LabformulierGenererenPartitioner partitioner, TaskExecutorPartitionHandler labformulierenGenererenPartitionHandler,
		Step labformulierenGenererenStep)
	{
		return new StepBuilder("labformulierenGenererenPartitionerStep", repository)
			.partitioner("labformulierenGenererenStep", partitioner)
			.partitionHandler(labformulierenGenererenPartitionHandler)
			.step(labformulierenGenererenStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler regioBrievenPartitionHandler(TaskExecutor taskExecutor, Step regioBrievenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(regioBrievenGenererenStep);
		return partitionHandler;
	}

	@Bean
	public TaskExecutorPartitionHandler labformulierenGenererenPartitionHandler(TaskExecutor taskExecutor, Step labformulierenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(labformulierenGenererenStep);
		return partitionHandler;
	}

	@Bean
	public Step regioBrievenControleStep(RegioBrievenControleReader reader, RegioBrievenControleWriter writer)
	{
		return new StepBuilder("regioBrievenControleStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step regioBrievenGenererenStep(RegioBrievenGenererenReader reader, RegioBrievenGenererenProcessor processor, RegioBrievenGenererenWriter writer)
	{
		return new StepBuilder("regioBrievenGenererenStep", repository)
			.<Long, CervixRegioBrief> chunk(250, transactionManager)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public Step labformulierenGenererenStep(LabformulierGenererenReader reader, LabformulierGenererenWriter writer)
	{
		return new StepBuilder("labformulierenGenererenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
