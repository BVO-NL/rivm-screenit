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
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.inactiveren.step.CervixCambiosInactiverenDecider;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.inactiveren.step.CervixCambiosInactiverenReader;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.inactiveren.step.CervixCambiosInactiverenWriter;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.uitstrijkje.step.CervixCambiosInclusieUitstrijkjeDecider;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.uitstrijkje.step.CervixCambiosInclusieUitstrijkjeReader;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.uitstrijkje.step.CervixCambiosInclusieUitstrijkjeWriter;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.zas.step.CervixCambiosInclusieZasDecider;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.zas.step.CervixCambiosInclusieZasReader;
import nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie.zas.step.CervixCambiosInclusieZasWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.job.Job;
import org.springframework.batch.core.job.builder.FlowBuilder;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.flow.Flow;
import org.springframework.batch.core.job.flow.support.SimpleFlow;
import org.springframework.batch.core.step.Step;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixCambiosInclusieJobConfiguration extends AbstractJobConfiguration
{
	@Bean
	public Job cambiosInclusieJob(CervixCambiosInclusieJobListener listener,
		Flow cambiosUitstrijkjeInclusieFlow, Flow cambiosZasInclusieFlow, Flow cambiosInactiverenFlow)
	{
		return new JobBuilder(JobType.CERVIX_CAMBIOS_INCLUSIE.name(), repository)
			.listener(listener)
			.start(cambiosUitstrijkjeInclusieFlow)
			.next(cambiosZasInclusieFlow)
			.next(cambiosInactiverenFlow)
			.end().build();
	}

	@Bean
	public Flow cambiosUitstrijkjeInclusieFlow(Step dummyStep, CervixCambiosInclusieUitstrijkjeDecider cambiosInclusieUitstrijkjeDecider, Step cambiosInclusieUitstrijkjeStep)
	{
		return new FlowBuilder<SimpleFlow>("cambiosUitstrijkjeInclusieFlow")
			.start(dummyStep)
			.next(cambiosInclusieUitstrijkjeDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(cambiosInclusieUitstrijkjeStep)
			.from(cambiosInclusieUitstrijkjeDecider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.build();
	}

	@Bean
	public Step cambiosInclusieUitstrijkjeStep(CervixCambiosInclusieUitstrijkjeReader reader, CervixCambiosInclusieUitstrijkjeWriter writer)
	{
		return new StepBuilder("cambiosInclusieUitstrijkjeStep", repository)
			.<Long, Long> chunk(1)
			.transactionManager(transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow cambiosZasInclusieFlow(Step dummyStep, CervixCambiosInclusieZasDecider cambiosZasInclusieDecider, Step cambiosZasInclusieStep)
	{
		return new FlowBuilder<SimpleFlow>("cambiosZasInclusieFlow")
			.start(dummyStep)
			.next(cambiosZasInclusieDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(cambiosZasInclusieStep)
			.from(cambiosZasInclusieDecider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.build();
	}

	@Bean
	public Step cambiosZasInclusieStep(CervixCambiosInclusieZasReader reader, CervixCambiosInclusieZasWriter writer)
	{
		return new StepBuilder("cambiosZasInclusieStep", repository)
			.<Long, Long> chunk(1)
			.transactionManager(transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow cambiosInactiverenFlow(Step dummyStep, CervixCambiosInactiverenDecider cambiosInactiverenDecider, Step cambiosInactiverenStep)
	{
		return new FlowBuilder<SimpleFlow>("cambiosInactiverenFlow")
			.start(dummyStep)
			.next(cambiosInactiverenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(cambiosInactiverenStep)
			.from(cambiosInactiverenDecider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.build();
	}

	@Bean
	public Step cambiosInactiverenStep(CervixCambiosInactiverenReader reader, CervixCambiosInactiverenWriter writer)
	{
		return new StepBuilder("cambiosInactiverenStep", repository)
			.<Long, Long> chunk(1)
			.transactionManager(transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
