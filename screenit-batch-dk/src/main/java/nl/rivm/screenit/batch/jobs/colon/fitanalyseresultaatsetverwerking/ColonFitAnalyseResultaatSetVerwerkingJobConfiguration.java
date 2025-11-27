package nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking;

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
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.analyseresultaatopruimenstep.ColonFitAnalyseResultaatOpruimenReader;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.analyseresultaatopruimenstep.ColonFitAnalyseResultaatOpruimenWriter;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.analyseresultaatsetopruimenstep.ColonFitAnalyseResultaatSetOpruimenReader;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.analyseresultaatsetopruimenstep.ColonFitAnalyseResultaatSetOpruimenWriter;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.verwerkingstep.ColonFitAnalyseResultaatSetVerwerkingProcessor;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.verwerkingstep.ColonFitAnalyseResultaatSetVerwerkingReader;
import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.verwerkingstep.ColonFitAnalyseResultaatSetVerwerkingWriter;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ColonFitAnalyseResultaatSetVerwerkingJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job fitAnalyseResultaatSetVerwerkingJob(ColonFitAnalyseResultaatSetVerwerkingListener listener, Step fitAnalyseResultaatSetVerwerkingStep,
		Step fitAnalyseResultaatOpruimenStep,
		Step fitAnalyseResultaatSetOpruimenStep)
	{
		return new JobBuilder(JobType.COLON_FIT_VERWERKING.name(), repository)
			.listener(listener)
			.start(fitAnalyseResultaatSetVerwerkingStep)
			.next(fitAnalyseResultaatOpruimenStep)
			.next(fitAnalyseResultaatSetOpruimenStep)
			.build();
	}

	@Bean
	public Step fitAnalyseResultaatSetVerwerkingStep(ColonFitAnalyseResultaatSetVerwerkingReader reader, ColonFitAnalyseResultaatSetVerwerkingProcessor processor,
		ColonFitAnalyseResultaatSetVerwerkingWriter writer)
	{
		return new StepBuilder("fitAnalyseResultaatSetVerwerkingStep", repository)
			.<Long, ColonFitAnalyseResultaat> chunk(10, transactionManager)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public Step fitAnalyseResultaatOpruimenStep(ColonFitAnalyseResultaatOpruimenReader reader, ColonFitAnalyseResultaatOpruimenWriter writer)
	{
		return new StepBuilder("fitAnalyseResultaatOpruimenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step fitAnalyseResultaatSetOpruimenStep(ColonFitAnalyseResultaatSetOpruimenReader reader, ColonFitAnalyseResultaatSetOpruimenWriter writer)
	{
		return new StepBuilder("fitAnalyseResultaatSetOpruimenStep", repository)
			.<Long, Long> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
