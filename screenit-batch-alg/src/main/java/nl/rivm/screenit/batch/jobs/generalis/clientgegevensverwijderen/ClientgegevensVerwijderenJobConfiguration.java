package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen;

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
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.clientgegevensverwijderen.ClientgegevensVerwijderenReader;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.clientgegevensverwijderen.ClientgegevensVerwijderenWriter;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.oudedossierslegen.OudeDossiersLegenReader;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.oudedossierslegen.OudeDossiersLegenWriter;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.persoonsgegevensverwijderen.PersoonsgegevensVerwijderenReader;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.persoonsgegevensverwijderen.PersoonsgegevensVerwijderenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ClientgegevensVerwijderenJobConfiguration extends AbstractJobConfiguration
{
	@Bean
	public Job clientgegevensVerwijderenJob(ClientgegevensVerwijderenListener listener, Step persoonsgegevensVerwijderenStep, Step clientgegevensVerwijderenStep,
		Step oudeDossiersLegenStep)
	{
		return new JobBuilder(JobType.CLIENTGEGEVENS_VERWIJDEREN.name(), repository)
			.listener(listener)
			.start(oudeDossiersLegenStep)
			.next(persoonsgegevensVerwijderenStep)
			.next(clientgegevensVerwijderenStep)
			.build();
	}

	@Bean
	public Step oudeDossiersLegenStep(OudeDossiersLegenReader reader, OudeDossiersLegenWriter writer)
	{
		return new StepBuilder("oudeDossiersLegen", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step persoonsgegevensVerwijderenStep(PersoonsgegevensVerwijderenReader reader, PersoonsgegevensVerwijderenWriter writer)
	{
		return new StepBuilder("persoonsgegevensVerwijderenStep", repository)
			.<Long, Long> chunk(1, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step clientgegevensVerwijderenStep(ClientgegevensVerwijderenReader reader, ClientgegevensVerwijderenWriter writer)
	{
		return new StepBuilder("clientgegevensVerwijderenStep", repository)
			.<Long, Long> chunk(1, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
