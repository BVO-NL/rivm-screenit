package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaAfspraakEventReader;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaAfspraakEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaAfspraakSampleReader;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaAfspraakSampleWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaFitAfspraakClassifierTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken.MammaPredictAfsprakenTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaFitDossierClassifierTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaPredictDossiersTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaScreeningRondeEventReader;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaScreeningRondeEventWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaScreeningRondeSampleReader;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.dossiers.MammaScreeningRondeSampleWriter;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.gemiddelden.MammaGemiddeldenTasklet;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.FlowBuilder;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.flow.Flow;
import org.springframework.batch.core.job.flow.support.SimpleFlow;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class MammaKansberekeningJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job kansberekeningJob(MammaKansberekeningListener listener, Flow samplesFlow, Flow eventsFlow, Flow predictFlow)
	{
		return new JobBuilder(JobType.MAMMA_KANSBEREKENING.name(), repository)
			.listener(listener)
			.start(samplesFlow)
			.next(eventsFlow)
			.next(predictFlow)
			.end().build();
	}

	@Bean
	public Flow samplesFlow(Step rondeSamplesStep, Step afspraakSamplesStep)
	{
		return new FlowBuilder<SimpleFlow>("samplesFlow")
			.start(rondeSamplesStep)
			.next(afspraakSamplesStep)
			.build();
	}

	@Bean
	public Flow eventsFlow(TaskExecutor taskExecutorKansberekening, Flow rondeEventsFlow, Flow afspraakEventsFlow, Flow gemiddeldenFlow)
	{
		return new FlowBuilder<SimpleFlow>("eventsFlow")
			.split(taskExecutorKansberekening)
			.add(rondeEventsFlow, afspraakEventsFlow, gemiddeldenFlow)
			.build();
	}

	@Bean
	public Flow predictFlow(Step dossierFitStep, Step dossierPredictStep, Step afspraakFitStep, Step afspraakPredictStep)
	{
		return new FlowBuilder<SimpleFlow>("predictFlow")
			.start(dossierFitStep)
			.next(dossierPredictStep)
			.next(afspraakFitStep)
			.next(afspraakPredictStep)
			.build();
	}

	@Bean
	public Step rondeSamplesStep(MammaScreeningRondeSampleReader reader, MammaScreeningRondeSampleWriter writer)
	{
		return new StepBuilder("rondeSamplesStep", repository)
			.<Long, Long> chunk(500, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step afspraakSamplesStep(MammaAfspraakSampleReader reader, MammaAfspraakSampleWriter writer)
	{
		return new StepBuilder("afspraakSamplesStep", repository)
			.<Long, Long> chunk(500, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow rondeEventsFlow(Step rondeEventsStep)
	{
		return new FlowBuilder<SimpleFlow>("rondeEventsFlow")
			.start(rondeEventsStep)
			.build();
	}

	@Bean
	public Step rondeEventsStep(MammaScreeningRondeEventReader reader, MammaScreeningRondeEventWriter writer)
	{
		return new StepBuilder("rondeEventsStep", repository)
			.<Long, Long> chunk(500, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow afspraakEventsFlow(Step afspraakEventsStep)
	{
		return new FlowBuilder<SimpleFlow>("afspraakEventsFlow")
			.start(afspraakEventsStep)
			.build();
	}

	@Bean
	public Step afspraakEventsStep(MammaAfspraakEventReader reader, MammaAfspraakEventWriter writer)
	{
		return new StepBuilder("afspraakEventsStep", repository)
			.<Long, Long> chunk(500, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow gemiddeldenFlow(Step gemiddeldenStep)
	{
		return new FlowBuilder<SimpleFlow>("gemiddeldenFlow")
			.start(gemiddeldenStep)
			.build();
	}

	@Bean
	public Step gemiddeldenStep(MammaGemiddeldenTasklet tasklet)
	{
		return new StepBuilder("gemiddeldenStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

	@Bean
	public Step dossierFitStep(MammaFitDossierClassifierTasklet tasklet)
	{
		return new StepBuilder("dossierFitStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

	@Bean
	public Step dossierPredictStep(MammaPredictDossiersTasklet tasklet)
	{
		return new StepBuilder("dossierPredictStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

	@Bean
	public Step afspraakFitStep(MammaFitAfspraakClassifierTasklet tasklet)
	{
		return new StepBuilder("afspraakFitStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

	@Bean
	public Step afspraakPredictStep(MammaPredictAfsprakenTasklet tasklet)
	{
		return new StepBuilder("afspraakPredictStep", repository)
			.tasklet(tasklet, transactionManager)
			.build();
	}

	@Bean
	public TaskExecutor taskExecutorKansberekening()
	{
		return new SimpleAsyncTaskExecutor();
	}

}
