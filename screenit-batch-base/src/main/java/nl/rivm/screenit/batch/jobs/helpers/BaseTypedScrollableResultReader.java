package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.ArrayList;
import java.util.List;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.model.enums.Level;

import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Slf4j
public abstract class BaseTypedScrollableResultReader<T> implements ItemReader<T>, ItemStream
{

	protected final ThreadLocal<ScrollableResults> resultSet = new ThreadLocal<>();

	protected final List<Long> processedIds = new ArrayList<>();

	@Setter
	protected int fetchSize = 20;

	@Autowired
	private SessionFactory sessionFactory;

	private boolean unbindSessionFromThread = false;

	@Getter(AccessLevel.PROTECTED)
	private Session hibernateSession;

	@Getter
	private StepExecution stepExecution;

	private JobExecution jobExecution;

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		try
		{

			hibernateSession = sessionFactory.openSession();
			if (!TransactionSynchronizationManager.hasResource(sessionFactory))
			{
				TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
				unbindSessionFromThread = true;
			}

			resultSet.set(createScrollableResults());
			processedIds.clear();
		}
		catch (IllegalStateException e)
		{
			crashMelding(e.getMessage(), e);
			throw e;
		}
		catch (Exception e)
		{
			crashMelding("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.", e);
			throw e;
		}
		finally
		{
			if (unbindSessionFromThread)
			{
				TransactionSynchronizationManager.unbindResource(sessionFactory);
			}
		}
	}

	protected abstract ScrollableResults createScrollableResults();

	@Override
	public void close() throws ItemStreamException
	{
		var scrollableResults = resultSet.get();
		if (scrollableResults != null)
		{
			scrollableResults.close();
		}
		if (hibernateSession != null)
		{
			SessionFactoryUtils.closeSession(hibernateSession);
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	protected void crashMelding(String melding, Exception exception)
	{
		LOG.error(melding, exception);
		if (!getExecutionContext().containsKey(BatchConstants.MELDING) || !Level.ERROR.equals(getExecutionContext().get(BatchConstants.LEVEL)))
		{
			getExecutionContext().put(BatchConstants.MELDING, melding);
			getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
		}
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	public ExecutionContext getStepExecutionContext()
	{
		return stepExecution.getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		this.jobExecution = stepExecution.getJobExecution();
	}
}
