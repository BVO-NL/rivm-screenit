package nl.rivm.screenit.huisartsenportaal.repository.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;

@Transactional(propagation = Propagation.SUPPORTS)
public abstract class BaseCustomRepositoryImpl<T>
{

	@Autowired
	private EntityManager entityManager;

	protected CriteriaBuilder getCriteriaBuilder()
	{
		return entityManager.getCriteriaBuilder();
	}

	protected T getSingleResult(CriteriaQuery<T> crit)
	{
		return entityManager.createQuery(crit).getSingleResult();
	}

	protected List<T> getResultList(CriteriaQuery<T> crit)
	{
		return entityManager.createQuery(crit).getResultList();
	}

	protected List<T> getResultList(CriteriaQuery<T> crit, int first, int max)
	{
		TypedQuery<T> typedQuery = entityManager.createQuery(crit);
		typedQuery.setFirstResult(first);
		typedQuery.setMaxResults(max);
		return typedQuery.getResultList();
	}

	public EntityManager getEntityManager()
	{
		return entityManager;
	}
}
