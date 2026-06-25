package nl.rivm.screenit.service.impl;

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

import jakarta.persistence.EntityManagerFactory;

import nl.rivm.screenit.service.DatabaseRunner;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.jpa.EntityManagerFactoryUtils;
import org.springframework.orm.jpa.EntityManagerHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Service
public class DatabaseRunnerImpl implements DatabaseRunner
{
	@Autowired
	private EntityManagerFactory entityManagerFactory;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void runInNewTransaction(ThrowingRunnable runnable)
	{
		try
		{
			runnable.run();
		}
		catch (Throwable e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void runInSessionOnly(ThrowingRunnable runnable)
	{
		var bindEntityManager = false;
		try
		{
			if (!TransactionSynchronizationManager.hasResource(entityManagerFactory))
			{
				var em = entityManagerFactory.createEntityManager();
				var emHolder = new EntityManagerHolder(em);
				TransactionSynchronizationManager.bindResource(entityManagerFactory, emHolder);
				bindEntityManager = true;
			}
			runnable.run();
		}
		catch (Throwable e)
		{
			throw new RuntimeException(e);
		}
		finally
		{
			if (bindEntityManager)
			{
				var emHolder = (EntityManagerHolder) TransactionSynchronizationManager.unbindResource(entityManagerFactory);
				EntityManagerFactoryUtils.closeEntityManager(emHolder.getEntityManager());
			}
		}
	}
}
