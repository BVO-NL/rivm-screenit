package nl.rivm.screenit.batch.jobs.mamma.palga.csvimport;

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
import nl.rivm.screenit.batch.jobs.mamma.palga.csvimport.step.MammaPalgaCsvImportReader;
import nl.rivm.screenit.batch.jobs.mamma.palga.csvimport.step.MammaPalgaCsvImportWriter;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaPalgaCsvImportJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job palgaCsvImportJob(MammaPalgaCsvImportListener listener, Step importerenStep)
	{
		return new JobBuilder(JobType.MAMMA_PALGA_CSV_IMPORT.name(), repository)
			.listener(listener)
			.start(importerenStep)
			.build();
	}

	@Bean
	public Step importerenStep(MammaPalgaCsvImportReader reader, MammaPalgaCsvImportWriter writer)
	{
		return new StepBuilder("importerenStep", repository)
			.<MammaPalgaCsvImportDto, MammaPalgaCsvImportDto> chunk(1, transactionManager)
			.reader(reader)
			.writer(writer)
			.faultTolerant()
			.noRollback(Exception.class)
			.build();
	}

	@Bean
	public MammaPalgaCsvImportReader mammaPalgaCsvImportReader(MammaPalgaCsvImportProvider provider)
	{
		var reader = new MammaPalgaCsvImportReader();
		reader.setCsvFileProvider(provider);
		return reader;
	}

}
