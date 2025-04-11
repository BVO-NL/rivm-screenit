package nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie;

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
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie.step.MammaDiscrepantieNaarArbitrageReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie.step.MammaDiscrepantieNaarArbitrageWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaDiscrepantieNaarArbitrageJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job beoordelingenDoorzettenNaarArbitrageJob(MammaDiscrepantieNaarArbitrageListener listener, Step beoordelingenDoorzettenNaarArbitrageStep)
	{
		return new JobBuilder(JobType.MAMMA_BEOORDELINGEN_DOORZETTEN_ARBITRAGE.name(), repository)
			.listener(listener)
			.start(beoordelingenDoorzettenNaarArbitrageStep)
			.build();
	}

	@Bean
	public Step beoordelingenDoorzettenNaarArbitrageStep(MammaDiscrepantieNaarArbitrageReader reader, MammaDiscrepantieNaarArbitrageWriter writer)
	{
		return new StepBuilder("beoordelingenDoorzettenNaarArbitrageStep", repository)
			.<Long, Long> chunk(50, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
