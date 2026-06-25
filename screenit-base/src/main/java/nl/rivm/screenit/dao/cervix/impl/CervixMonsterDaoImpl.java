package nl.rivm.screenit.dao.cervix.impl;

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

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;

import org.hibernate.Session;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
public class CervixMonsterDaoImpl implements CervixMonsterDao
{
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	@Transactional
	public Long getNextMonsterId()
	{
		var session = entityManager.unwrap(Session.class);
		return session.doReturningWork(new SequenceGenerator(DatabaseSequence.MONSTER_ID, entityManager.getEntityManagerFactory()));
	}
}
