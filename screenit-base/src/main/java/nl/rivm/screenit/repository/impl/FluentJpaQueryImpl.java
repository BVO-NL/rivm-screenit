package nl.rivm.screenit.repository.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import jakarta.persistence.EntityGraph;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Tuple;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Selection;

import nl.rivm.screenit.repository.FluentJpaQuery;
import nl.rivm.screenit.util.functionalinterfaces.TriFunction;

import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.boot.internal.SessionFactoryOptionsBuilder;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.internal.EmptyScrollableResults;
import org.hibernate.query.Query;
import org.jetbrains.annotations.NotNull;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.query.QueryUtils;
import org.springframework.util.Assert;

import com.google.common.primitives.Ints;

public class FluentJpaQueryImpl<T, P> implements FluentJpaQuery<T, P>
{
	private final Specification<T> specification;

	private final EntityManager entityManager;

	private final Class<P> projectionType;

	private final Class<T> entityType;

	private boolean distinctOn;

	private BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> projectionFunction;

	private BiFunction<CriteriaBuilder, Root<T>, List<Expression<?>>> groupByFunction;

	private Sort sort = Sort.unsorted();

	private TriFunction<Sort.Order, Root<T>, CriteriaBuilder, Order> sortFunction;

	private BiFunction<Root<T>, CriteriaBuilder, List<Order>> ordersFunction;

	private EntityGraph<T> entityGraph;

	private int maxEntityGraphDepth;

	private int fetchSize;

	public FluentJpaQueryImpl(Specification<T> specification, EntityManager entityManager, Class<T> entityType, Class<P> projectionType)
	{
		this.specification = specification;
		this.entityManager = entityManager;
		this.projectionType = projectionType;
		this.entityType = entityType;
	}

	@Override
	public FluentJpaQuery<T, P> sortBy(Sort sort)
	{
		Assert.isNull(this.ordersFunction, "Only one of sortBy() may be called.");
		this.sort = this.sort.and(sort);
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> sortBy(Sort sort, TriFunction<Sort.Order, Root<T>, CriteriaBuilder, Order> sortFunction)
	{
		sortBy(sort);
		this.sortFunction = sortFunction;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> sortBy(BiFunction<Root<T>, CriteriaBuilder, List<Order>> ordersFunction)
	{
		Assert.isNull(this.ordersFunction, "Only one of sortBy() may be called.");
		Assert.isTrue(this.sort.isUnsorted(), "Only one of sortBy() may be called.");
		this.ordersFunction = ordersFunction;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> projection(BiFunction<CriteriaBuilder, Root<T>, Selection<?>> projectionFunction)
	{
		Assert.isNull(this.projectionFunction, "Only one of projection() or projections() may be called and only a single time. Use projections() for multiple projections.");
		this.projectionFunction = (cb, r) -> List.of(projectionFunction.apply(cb, r));
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> projections(BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> projectionFunction)
	{
		Assert.isNull(this.projectionFunction, "Only one of projection() or projections() may be called and only a single time.");
		this.projectionFunction = projectionFunction;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> groupBy(BiFunction<CriteriaBuilder, Root<T>, List<Expression<?>>> groupByFunction)
	{
		Assert.isNull(this.groupByFunction, "groupBy() may only be called a single time.");
		this.groupByFunction = groupByFunction;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> distinct()
	{
		distinctOn = true;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> fetch(Consumer<EntityGraph<T>> entityGraphFunction)
	{
		return fetch(entityGraphFunction, getDefaultMaxFetchDepth());
	}

	@Override
	public FluentJpaQuery<T, P> fetch(Consumer<EntityGraph<T>> entityGraphFunction, int entityGraphDepth)
	{
		this.maxEntityGraphDepth = Math.max(this.maxEntityGraphDepth, entityGraphDepth);
		if (entityGraph == null)
		{
			entityGraph = entityManager.createEntityGraph(entityType);
		}
		entityGraphFunction.accept(this.entityGraph);

		return this;
	}

	@Override
	public FluentJpaQuery<T, P> setScrollFetchSize(int fetchSize)
	{
		this.fetchSize = fetchSize;
		return this;
	}

	@Override
	public List<P> all()
	{
		var oldGraphDepth = applyEntityFetchGraphDepth();
		try
		{
			var typedQuery = getTypedQuery();
			return typedQuery.getResultList();
		}
		finally
		{
			resetEntityFetchGraphDepth(oldGraphDepth);
		}
	}

	private TypedQuery<P> getTypedQuery()
	{
		return createTypedQuery();
	}

	@Override
	public List<P> all(long first, long count)
	{
		var query = createTypedQuery();
		if (first > -1)
		{
			query.setFirstResult(Ints.checkedCast(first));
		}

		if (count > -1)
		{
			query.setMaxResults(Ints.checkedCast(count));
		}
		var oldGraphDepth = applyEntityFetchGraphDepth();
		try
		{
			return query.getResultList();
		}
		finally
		{
			resetEntityFetchGraphDepth(oldGraphDepth);
		}
	}

	@Override
	public Optional<P> first()
	{
		Assert.isTrue(sort.isSorted(), "Sorting is required for first()");
		var oldGraphDepth = applyEntityFetchGraphDepth();
		try
		{
			return createTypedQuery()
				.setMaxResults(1)
				.getResultList()
				.stream()
				.findFirst();
		}
		finally
		{
			resetEntityFetchGraphDepth(oldGraphDepth);
		}
	}

	@Override
	public Optional<P> one()
	{
		var oldGraphDepth = applyEntityFetchGraphDepth();
		try
		{
			var results = createTypedQuery()
				.setMaxResults(2)
				.getResultList();
			if (results.size() > 1)
			{
				throw new IncorrectResultSizeDataAccessException(1);
			}
			return results.stream().filter(Objects::nonNull).findFirst();
		}
		finally
		{
			resetEntityFetchGraphDepth(oldGraphDepth);
		}
	}

	@Override
	public ScrollableResults scroll(Integer maxResults)
	{
		var typedQuery = createTypedQuery();
		if (maxResults != null)
		{
			if (maxResults > 0)
			{
				typedQuery.setMaxResults(maxResults);
			}
			else if (maxResults == 0)
			{
				return new EmptyScrollableResults();
			}
		}
		if (typedQuery instanceof Query)
		{
			var oldGraphDepth = applyEntityFetchGraphDepth();
			try
			{
				return ((Query<P>) typedQuery).setFetchSize(fetchSize).scroll(ScrollMode.FORWARD_ONLY);
			}
			finally
			{
				resetEntityFetchGraphDepth(oldGraphDepth);
			}
		}
		return new EmptyScrollableResults();
	}

	private TypedQuery<P> createTypedQuery()
	{
		var cb = entityManager.getCriteriaBuilder();
		var q = cb.createQuery(projectionType);
		var r = q.from(entityType);

		addWhereClause(r, q, cb);
		addSelections(r, q, cb); 
		addGroupBy(r, q, cb);
		addOrderBy(r, q, cb);
		q.distinct(distinctOn);

		var typedQuery = entityManager.createQuery(q);
		addFetchGraph(typedQuery);
		return typedQuery;
	}

	private void addWhereClause(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (specification != null)
		{
			var predicate = specification.toPredicate(r, q, cb);
			if (predicate != null)
			{
				q.where(predicate);
			}
		}
	}

	private void addSelections(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (projectionFunction != null)
		{
			var selections = projectionFunction.apply(cb, r).toArray(Selection<?>[]::new);

			if (selections.length == 1)
			{
				q.select((Selection<? extends P>) selections[0]);
			}
			else if (projectionType.isArray() || projectionType == Tuple.class)
			{
				q.multiselect(selections);
			}
			else
			{
				q.select(cb.construct(projectionType, selections));
			}
		}
	}

	private void addGroupBy(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (groupByFunction != null)
		{
			q.groupBy(groupByFunction.apply(cb, r));
		}
	}

	private void addOrderBy(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (sort.isSorted())
		{
			q.orderBy(getOrders(sort, r, cb));
		}
		else if (ordersFunction != null)
		{
			q.orderBy(ordersFunction.apply(r, cb).stream().filter(Objects::nonNull).collect(Collectors.toList()));
		}
	}

	private @NotNull List<Order> getOrders(Sort sort, Root<T> r, CriteriaBuilder cb)
	{
		var orders = new ArrayList<Order>();
		var splittedSortOrders = new ArrayList<Sort.Order>();
		for (var order : sort)
		{
			var correctedOrder = sortFunction != null ? sortFunction.apply(order, r, cb) : null;
			if (correctedOrder == null)
			{
				splittedSortOrders.add(order);
			}
			else
			{
				orders.addAll(QueryUtils.toOrders(Sort.by(splittedSortOrders), r, cb));
				orders.add(correctedOrder);
				splittedSortOrders.clear();
			}
		}
		orders.addAll(QueryUtils.toOrders(Sort.by(splittedSortOrders), r, cb));
		return orders;
	}

	private void addFetchGraph(TypedQuery<P> typedQuery)
	{
		if (entityGraph != null)
		{
			typedQuery.setHint("jakarta.persistence.loadgraph", entityGraph);
		}
	}

	private int getDefaultMaxFetchDepth()
	{
		return getSessionFactoryOptionsBuilder().getMaximumFetchDepth();
	}

	private @NotNull Integer applyEntityFetchGraphDepth()
	{
		var sfob = getSessionFactoryOptionsBuilder();
		var oldGraphDepth = sfob.getMaximumFetchDepth();
		if (entityGraph != null)
		{
			sfob.applyMaximumFetchDepth(maxEntityGraphDepth);
		}
		return oldGraphDepth;
	}

	private void resetEntityFetchGraphDepth(Integer oldGraphDepth)
	{
		var sfob = getSessionFactoryOptionsBuilder();
		sfob.applyMaximumFetchDepth(oldGraphDepth);
	}

	private @NotNull SessionFactoryOptionsBuilder getSessionFactoryOptionsBuilder()
	{
		var sfi = entityManager.getEntityManagerFactory().unwrap(SessionFactoryImplementor.class);
		return (SessionFactoryOptionsBuilder) sfi.getSessionFactoryOptions();
	}
}
