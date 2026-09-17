package nl.rivm.screenit.specification;

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

import java.io.Serializable;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Predicate;

import org.jspecify.annotations.Nullable;

class ExtendedSpecificationComposition
{
	interface Combiner extends Serializable
	{
		Predicate combine(CriteriaBuilder builder, @Nullable Predicate links, @Nullable Predicate rechts);
	}

	static <T> ExtendedSpecification<T> composed(@Nullable ExtendedSpecification<T> links, @Nullable ExtendedSpecification<T> rechts, Combiner combiner)
	{
		return (from, query, builder) ->
		{
			var linkerPredicate = toPredicate(links, from, query, builder);
			var rechterPredicate = toPredicate(rechts, from, query, builder);
			if (linkerPredicate == null)
			{
				return rechterPredicate;
			}
			return rechterPredicate == null ? linkerPredicate : combiner.combine(builder, linkerPredicate, rechterPredicate);
		};
	}

	@Nullable
	private static <T> Predicate toPredicate(@Nullable ExtendedSpecification<T> specification, From<?, ? extends T> from, CriteriaQuery<?> query,
		CriteriaBuilder builder)
	{
		return specification == null ? null : specification.toPredicate(from, query, builder);
	}
}
