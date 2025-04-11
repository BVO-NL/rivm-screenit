package nl.rivm.screenit.specification;

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

import java.util.function.Function;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.ListAttribute;
import jakarta.persistence.metamodel.SingularAttribute;

import org.springframework.data.jpa.domain.Specification;

@FunctionalInterface
public interface ExtendedSpecification<T> extends Specification<T>
{
	Predicate toPredicate(From<?, ? extends T> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder);

	@Override
	default Predicate toPredicate(Root<T> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder)
	{
		return toPredicate((From<?, ? extends T>) root, query, criteriaBuilder);
	}

	default <S> ExtendedSpecification<S> with(SingularAttribute<S, ? extends T> attribute)
	{
		return with(attribute, JoinType.INNER);
	}

	default <S> ExtendedSpecification<S> with(SingularAttribute<S, ? extends T> attribute, JoinType joinType)
	{
		return with(r -> SpecificationUtil.join(r, attribute, joinType));
	}

	default <S> ExtendedSpecification<S> with(ListAttribute<S, ? extends T> attribute)
	{
		return with(attribute, JoinType.INNER);
	}

	default <S> ExtendedSpecification<S> with(ListAttribute<S, ? extends T> attribute, JoinType joinType)
	{
		return with(r -> SpecificationUtil.join(r, attribute, joinType));
	}

	default <S> ExtendedSpecification<S> with(Function<From<?, ? extends S>, From<?, ? extends T>> joinFunction)
	{
		return (r, q, cb) -> toPredicate(joinFunction.apply(r), q, cb);
	}

	default <S> Specification<S> withRoot(Function<Root<S>, From<?, ? extends T>> joinFunction)
	{
		return (r, q, cb) -> toPredicate(joinFunction.apply(r), q, cb);
	}

	default ExtendedSpecification<T> and(ExtendedSpecification<T> other)
	{
		return (r, q, cb) ->
		{
			var thisPredicate = toPredicate(r, q, cb);
			var otherPredicate = other == null ? null : other.toPredicate(r, q, cb);
			if (thisPredicate == null)
			{
				return otherPredicate;
			}
			else
			{
				return otherPredicate == null ? thisPredicate : cb.and(thisPredicate, otherPredicate);
			}
		};
	}

	default ExtendedSpecification<T> or(ExtendedSpecification<T> other)
	{
		return (r, q, cb) ->
		{
			var thisPredicate = toPredicate(r, q, cb);
			var otherPredicate = other == null ? null : other.toPredicate(r, q, cb);
			if (thisPredicate == null)
			{
				return otherPredicate;
			}
			else
			{
				return otherPredicate == null ? thisPredicate : cb.or(thisPredicate, otherPredicate);
			}
		};
	}

	static <T> ExtendedSpecification<T> not(ExtendedSpecification<T> spec)
	{
		return (r, q, cb) -> spec == null ? null : cb.not(spec.toPredicate(r, q, cb));
	}
}
