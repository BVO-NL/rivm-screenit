package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.aanmaakstep.ProjectBrievenAanmaakReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.aanmaakstep.ProjectBrievenAanmaakWriter;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.cleanupstep.ProjectBriefCleanUpReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.cleanupstep.ProjectBriefCleanUpWriter;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.controle.ProjectBrievenControleReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.controle.ProjectBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep.ProjectBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep.ProjectBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep.ProjectBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep.ProjectBrievenGenererenWriter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.project.ProjectBrief;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class ProjectBrievenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job projectBrievenJob(ProjectBrievenListener listener, Step projectBrievenCleanupStep, Step projectBrievenKlaarzettenStep, Step projectBrievenPartitionerStep,
		Step projectBrievenControleStep)
	{
		return new JobBuilder(JobType.PROJECT_BRIEVEN.name(), repository)
			.listener(listener)
			.start(projectBrievenCleanupStep)
			.next(projectBrievenKlaarzettenStep)
			.next(projectBrievenPartitionerStep)
			.next(projectBrievenControleStep)
			.build();
	}

	@Bean
	public Step projectBrievenCleanupStep(ProjectBriefCleanUpReader reader, ProjectBriefCleanUpWriter writer)
	{
		return new StepBuilder("projectBrievenCleanupStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step projectBrievenKlaarzettenStep(ProjectBrievenAanmaakReader reader, ProjectBrievenAanmaakWriter writer)
	{
		return new StepBuilder("projectBrievenKlaarzettenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step projectBrievenPartitionerStep(ProjectBrievenGenererenPartitioner partitioner, Step projectBrievenGenererenStep,
		TaskExecutorPartitionHandler projectBrievenPartitionHandler)
	{
		return new StepBuilder("projectBrievenPartitionerStep", repository)
			.partitioner("projectBrievenGenererenStep", partitioner)
			.partitionHandler(projectBrievenPartitionHandler)
			.step(projectBrievenGenererenStep)
			.build();
	}

	@Bean
	public Step projectBrievenControleStep(ProjectBrievenControleReader reader, ProjectBrievenControleWriter writer)
	{
		return new StepBuilder("projectBrievenControleStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step projectBrievenGenererenStep(ProjectBrievenGenererenReader reader, ProjectBrievenGenererenProcessor processor, ProjectBrievenGenererenWriter writer)
	{
		return new StepBuilder("projectBrievenGenererenStep", repository)
			.<Long, ProjectBrief> chunk(250, transactionManager)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler projectBrievenPartitionHandler(TaskExecutor taskExecutor, Step projectBrievenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(projectBrievenGenererenStep);
		partitionHandler.setGridSize(10);
		return partitionHandler;
	}

}
