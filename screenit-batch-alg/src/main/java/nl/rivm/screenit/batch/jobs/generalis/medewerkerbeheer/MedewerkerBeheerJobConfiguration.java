package nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer;

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
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.mederwerkerinactiveren.MedewerkerInactiverenReader;
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.mederwerkerinactiveren.MedewerkerInactiverenWriter;
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.rolkoppelinginactiveren.RolKoppelingInactiverenReader;
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.rolkoppelinginactiveren.RolKoppelingInactiverenWriter;
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.wachtwoordverlooptherinnering.WachtwoordVerlooptHerinneringReader;
import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.wachtwoordverlooptherinnering.WachtwoordVerlooptHerinneringWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MedewerkerBeheerJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job medewerkerBeheerJob(MedewerkerBeheerListener listener, Step medewerkerInactiverenStep, Step rolKoppelingInactiverenStep)
	{
		return new JobBuilder(JobType.MEDEWERKER_BEHEER.name(), repository)
			.listener(listener)
			.start(medewerkerInactiverenStep)
			.next(rolKoppelingInactiverenStep)
			.build();
	}

	@Bean
	public Step wachtwoordVerlooptHerinneringStep(WachtwoordVerlooptHerinneringReader reader, WachtwoordVerlooptHerinneringWriter writer)
	{
		return new StepBuilder("wachtwoordVerlooptHerinneringStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step medewerkerInactiverenStep(MedewerkerInactiverenReader reader, MedewerkerInactiverenWriter writer)
	{
		return new StepBuilder("medewerkerInactiverenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step rolKoppelingInactiverenStep(RolKoppelingInactiverenReader reader, RolKoppelingInactiverenWriter writer)
	{
		return new StepBuilder("rolKoppelingInactiverenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
