package nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.genereren.OverdrachtPersoonsgegevensDataDumpGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.genereren.OverdrachtPersoonsgegevensDataDumpGenererenWriter;
import nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.opruimen.OverdrachtPersoonsgegevensDataDumpOpruimenReader;
import nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.opruimen.OverdrachtPersoonsgegevensDataDumpOpruimenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.job.Job;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.Step;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OverdrachtPersoonsgegevensDataDumpJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job overdrachtPersoonsgegevensDataDumpJob(OverdrachtPersoonsgegevensDataDumpListener listener, Step overdrachtPersoonsgegevensDataDumpGenererenStep,
		Step overdrachtPersoonsgegevensDataDumpOpruimenStep)
	{
		return new JobBuilder(JobType.OVERDRACHT_PERSOONSGEGEVENS_DATADUMP.name(), repository)
			.listener(listener)
			.start(overdrachtPersoonsgegevensDataDumpGenererenStep)
			.next(overdrachtPersoonsgegevensDataDumpOpruimenStep)
			.build();
	}

	@Bean
	public Step overdrachtPersoonsgegevensDataDumpGenererenStep(OverdrachtPersoonsgegevensDataDumpGenererenReader reader,
		OverdrachtPersoonsgegevensDataDumpGenererenWriter writer)
	{
		return new StepBuilder("overdrachtPersoonsgegevensDataDumpGenererenStep", repository)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.transactionManager(transactionManager)
			.build();
	}

	@Bean
	public Step overdrachtPersoonsgegevensDataDumpOpruimenStep(OverdrachtPersoonsgegevensDataDumpOpruimenReader reader,
		OverdrachtPersoonsgegevensDataDumpOpruimenWriter writer)
	{
		return new StepBuilder("overdrachtPersoonsgegevensDataDumpOpruimenStep", repository)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.transactionManager(transactionManager)
			.build();
	}

}
