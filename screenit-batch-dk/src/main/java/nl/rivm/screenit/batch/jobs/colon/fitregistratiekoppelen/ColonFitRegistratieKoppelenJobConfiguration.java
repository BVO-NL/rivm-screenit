package nl.rivm.screenit.batch.jobs.colon.fitregistratiekoppelen;

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
import nl.rivm.screenit.batch.jobs.colon.KoppelPromotionListener;
import nl.rivm.screenit.batch.jobs.colon.fitregistratiekoppelen.koppelmetreststep.ColonFitRegistratieKoppelenMetRestReader;
import nl.rivm.screenit.batch.jobs.colon.fitregistratiekoppelen.koppelmetreststep.ColonFitRegistratieKoppelenMetRestWriter;
import nl.rivm.screenit.batch.jobs.colon.fitregistratiekoppelen.koppelstep.ColonFitRegistratieKoppelenReader;
import nl.rivm.screenit.batch.jobs.colon.fitregistratiekoppelen.koppelstep.ColonFitRegistratieKoppelenWriter;
import nl.rivm.screenit.batch.jobs.helpers.BaseKoppelenDecider;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.service.impl.WebserviceInpakcentrumOpzettenServiceImpl;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import generated.KOPPELDATA;

@Configuration
public class ColonFitRegistratieKoppelenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job koppeldataVerwerkingJob(ColonFitRegistratieKoppelenListener listener, KoppelPromotionListener koppelPromotionListener, Step dummyStep,
		BaseKoppelenDecider koppelDecider,
		Step koppelenMetRestStep, Step koppelenStep)
	{
		return new JobBuilder(JobType.KOPPELDATA_VERWERKING.name(), repository)
			.listener(listener)
			.listener(koppelPromotionListener)
			.start(dummyStep)
			.next(koppelDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(koppelenMetRestStep)
			.from(koppelDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(koppelenStep).end()
			.build();
	}

	@Bean
	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	public Step koppelenStep(ColonFitRegistratieKoppelenReader reader, ColonFitRegistratieKoppelenWriter writer)
	{
		return new StepBuilder("koppelenStep", repository)
			.<KOPPELDATA.VERZONDENUITNODIGING, KOPPELDATA.VERZONDENUITNODIGING> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step koppelenMetRestStep(ColonFitRegistratieKoppelenMetRestReader reader, ColonFitRegistratieKoppelenMetRestWriter writer)
	{
		return new StepBuilder("koppelenMetRestStep", repository)
			.<InpakcentrumKoppelDataDto, InpakcentrumKoppelDataDto> chunk(250, transactionManager)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public BaseKoppelenDecider koppelDecider()
	{
		return new BaseKoppelenDecider();
	}

	@Bean
	public WebserviceInpakcentrumOpzettenService webserviceInpakcentrumOpzettenService()
	{
		return new WebserviceInpakcentrumOpzettenServiceImpl();
	}

	@Bean
	public ScreenITLoggingSaver screenITLoggingSaver()
	{
		return new ScreenITLoggingSaver();
	}
}
