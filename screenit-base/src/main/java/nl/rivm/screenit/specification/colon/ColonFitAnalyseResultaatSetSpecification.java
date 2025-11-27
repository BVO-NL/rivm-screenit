package nl.rivm.screenit.specification.colon;

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

import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet_;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat_;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFitAnalyseResultaatSetSpecification
{
	public static Specification<ColonFitAnalyseResultaatSet> heeftStatussen(List<ColonFitAnalyseResultaatSetStatus> statussen)
	{
		return (r, q, cb) -> r.get(ColonFitAnalyseResultaatSet_.status).in(statussen);
	}

	public static Specification<ColonFitAnalyseResultaatSet> heeftBestandsnaam(String bestandsnaam)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonFitAnalyseResultaatSet_.naamBestand), bestandsnaam);
	}

	public static ExtendedSpecification<ColonFitAnalyseResultaatSet> heeftStatus(ColonFitAnalyseResultaatSetStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonFitAnalyseResultaatSet_.status), status);
	}

	public static ExtendedSpecification<ColonFitAnalyseResultaatSet> filterStatus(ColonFitAnalyseResultaatSetStatus status)
	{
		return (r, q, cb) ->
		{
			List<ColonFitAnalyseResultaatSetStatus> statussen = List.of();
			if (status == null)
			{
				statussen = List.of(ColonFitAnalyseResultaatSetStatus.GEAUTORISEERD, ColonFitAnalyseResultaatSetStatus.INGELEZEN, ColonFitAnalyseResultaatSetStatus.VERWERKT);
			}
			else if (status == ColonFitAnalyseResultaatSetStatus.VERWERKT)
			{
				statussen = List.of(ColonFitAnalyseResultaatSetStatus.GEAUTORISEERD, ColonFitAnalyseResultaatSetStatus.VERWERKT);
			}
			else if (status == ColonFitAnalyseResultaatSetStatus.INGELEZEN)
			{
				statussen = List.of(ColonFitAnalyseResultaatSetStatus.INGELEZEN);
			}
			return r.get(ColonFitAnalyseResultaatSet_.status).in(statussen);
		};
	}

	public static ExtendedSpecification<ColonFitAnalyseResultaatSet> filterLaboratorium(ColonFitLaboratorium laboratorium)
	{
		return skipWhenNullExtended(laboratorium, (r, q, cb) -> cb.equal(r.get(ColonFitAnalyseResultaatSet_.laboratorium), laboratorium));
	}

	public static ExtendedSpecification<ColonFitAnalyseResultaatSet> heeftStatusDatumTussen(Range<Date> peilRange)
	{
		return bevat(peilRange, r -> r.get(ColonFitAnalyseResultaatSet_.statusDatum));
	}

	public static ExtendedSpecification<ColonFitAnalyseResultaatSet> heeftGeenUitslagen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(ColonFitAnalyseResultaat.class);
			subquery.select(cb.literal(1L))
				.where(cb.equal(subRoot.get(ColonFitAnalyseResultaat_.analyseResultaatSet).get(AbstractHibernateObject_.id), r.get(AbstractHibernateObject_.id)));
			return cb.not(cb.exists(subquery));
		};
	}
}
