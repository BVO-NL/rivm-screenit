package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.ParameterizedType;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.service.RevisionInformationService;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.job.JobExecution;
import org.springframework.batch.core.listener.JobExecutionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.REQUIRED)
public abstract class BaseLogListener implements JobExecutionListener
{

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private RevisionInformationService revisionInformationService;

	private JobExecution jobExecution = null;

	protected void beforeStarting(JobExecution jobExecution)
	{
	}

	@Override
	public void beforeJob(JobExecution jobExecution)
	{
		if (jobExecution != null)
		{
			this.jobExecution = jobExecution;
			jobExecution.setStartTime(currentDateSupplier.getLocalDateTime());
		}
		beforeStarting(jobExecution);
		saveStartLogGebeurtenis();
	}

	protected void saveStartLogGebeurtenis()
	{
		var startLogGebeurtenis = getStartLogGebeurtenis();
		var bevolkingsonderzoek = getBevolkingsonderzoek();
		var startLogEvent = getStartLogEvent();
		logService.logGebeurtenis(startLogGebeurtenis, startLogEvent, bevolkingsonderzoek);
	}

	protected abstract LogEvent getStartLogEvent();

	protected abstract LogGebeurtenis getStartLogGebeurtenis();

	@Override
	public void afterJob(JobExecution jobExecution)
	{
		if (jobExecution != null)
		{
			this.jobExecution = jobExecution;
		}
		beforeEindeLogging(jobExecution);
		var logEvent = eindLogging(jobExecution);
		saveEindLogGebeurtenis(logEvent);
	}

	protected void saveEindLogGebeurtenis(LogEvent logEvent)
	{
		var bevolkingsonderzoek = getBevolkingsonderzoek();
		var eindLogGebeurtenis = getEindLogGebeurtenis();

		logService.logGebeurtenis(eindLogGebeurtenis, logEvent, bevolkingsonderzoek);
	}

	protected abstract LogGebeurtenis getEindLogGebeurtenis();

	protected abstract LogEvent getEindLogEvent();

	protected abstract Bevolkingsonderzoek getBevolkingsonderzoek();

	protected JobType getJobType()
	{
		return JobType.valueOf(jobExecution.getJobInstance().getJobName().toUpperCase());
	}

	protected void beforeEindeLogging(JobExecution jobExecution)
	{
	}

	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = getEindLogEvent();
		var level = getLevel(jobExecution);
		addMelding(logEvent, getMelding(jobExecution));
		if (jobHasExitCode(ExitStatus.FAILED) || Level.ERROR.equals(level))
		{
			logEvent.setLevel(Level.ERROR);
			if (CollectionUtils.isNotEmpty(jobExecution.getAllFailureExceptions()))
			{
				addMelding(logEvent, "De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk");
			}
		}
		else if (Level.WARNING.equals(level))
		{
			logEvent.setLevel(Level.WARNING);
		}
		else
		{
			logEvent.setLevel(Level.INFO);
		}
		return logEvent;
	}

	protected static void addMelding(LogEvent logEvent, String melding)
	{
		var huidigeMelding = logEvent.getMelding();
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;

		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		logEvent.setMelding(huidigeMelding);
	}

	protected boolean jobHasExitCode(ExitStatus status)
	{
		return status != null && status.getExitCode() != null && jobExecution != null && jobExecution.getExitStatus() != null
			&& status.getExitCode().equals(jobExecution.getExitStatus().getExitCode());
	}

	protected String getStackTrace(Throwable aThrowable)
	{
		final Writer result = new StringWriter();
		final var printWriter = new PrintWriter(result);
		aThrowable.printStackTrace(printWriter);
		if (result.toString().length() > 4000)
		{
			return result.toString().substring(0, 4000);
		}
		return result.toString();
	}

	protected Level getLevel(JobExecution execution)
	{
		var level = Level.INFO;
		var context = execution.getExecutionContext();
		if (context.containsKey(BatchConstants.LEVEL))
		{
			level = (Level) context.get(BatchConstants.LEVEL);
		}
		return level;
	}

	protected String getMelding(JobExecution execution)
	{
		var melding = "";
		var context = execution.getExecutionContext();
		if (context.containsKey(BatchConstants.MELDING))
		{
			melding = context.getString(BatchConstants.MELDING);
		}
		return melding;
	}

	protected JobExecution getJobExecution()
	{
		return this.jobExecution;
	}

	protected <T> T getTypedValueFromExecutionContext(String key)
	{
		return (T) getJobExecution().getExecutionContext().get(key);
	}

	protected <E extends Enum> void aantallenContextVerwerken(String enumKey, AantalVerwerker<E> aantalVerwerker)
	{
		var context = getJobExecution().getExecutionContext();

		var enumConstants = ((Class<E>) ((ParameterizedType) aantalVerwerker.getClass().getGenericSuperclass()).getActualTypeArguments()[0]).getEnumConstants();
		for (var enumConstant : enumConstants)
		{
			var key = BaseWriter.getEnumKey(enumKey, enumConstant);
			if (context.containsKey(key))
			{
				var aantal = context.getLong(key);
				aantalVerwerker.verwerk(enumConstant, aantal);
			}
		}
	}

	protected void unregisterRevisionKenmerk(String context)
	{
		revisionInformationService.unregister(context);
	}

	protected void registerRevisionKenmerk(String context, RevisionKenmerk kenmerk)
	{
		revisionInformationService.registerKenmerk(context, kenmerk);
	}

	protected abstract static class AantalVerwerker<E extends Enum>
	{
		protected abstract void verwerk(E enumConstant, long aantal);
	}
}
