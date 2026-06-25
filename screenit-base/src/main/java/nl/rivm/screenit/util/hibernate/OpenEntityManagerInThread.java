package nl.rivm.screenit.util.hibernate;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.DatabaseRunner;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class OpenEntityManagerInThread extends Thread
{
	private final boolean withTransaction;

	@Autowired
	private DatabaseRunner databaseRunner;

	public OpenEntityManagerInThread()
	{
		this(false);
	}

	public OpenEntityManagerInThread(boolean withTransaction)
	{
		super();
		this.withTransaction = withTransaction;
		var applicationContext = ApplicationContextProvider.getApplicationContext();
		if (applicationContext != null)
		{
			applicationContext.getAutowireCapableBeanFactory().autowireBean(this);
		}
		else
		{
			LOG.warn("applicationContext niet gevonden, kan geen beans autowired injecten.");
		}
	}

	@Override
	public void run()
	{
		try
		{
			if (withTransaction)
			{
				databaseRunner.runInNewTransaction(() -> runInternal());
			}
			else
			{
				databaseRunner.runInSessionOnly(() -> runInternal());
			}
		}
		catch (Exception e)
		{
			LOG.error("Error in tread", e);
		}
	}

	protected abstract void runInternal();
}
