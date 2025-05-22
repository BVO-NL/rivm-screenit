package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.BezwaarMoment_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.algemeen.BezwaarBrief_;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.specification.algemeen.BezwaarMomentSpecification;
import nl.rivm.screenit.specification.algemeen.BezwaarSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

public abstract class AbstractGegevensVerwijderenReader extends BaseSpecificationScrollableResultReader<Client>
{
	@Override
	protected Specification<Client> createSpecification()
	{
		return ClientSpecification.heeftGbaStatus(GbaStatus.BEZWAAR)
			.and(BezwaarMomentSpecification.heeftBezwaarBrief().with(Client_.laatstVoltooideBezwaarMoment, JoinType.LEFT))
			.and(BezwaarSpecification.heeftBezwaarMetType(BezwaarType.GEEN_OPNAME_UIT_BPR).with(r ->
			{
				var bezwaarMomentJoin = getBezwaarMomentJoin(r);
				return join(bezwaarMomentJoin, BezwaarMoment_.bezwaren);
			}))
			.and(MergedBrievenSpecification.isVerstuurd().with(r ->
			{
				var bezwaarMomentJoin = getBezwaarMomentJoin(r);
				var bezwaarBriefJoin = join(bezwaarMomentJoin, BezwaarMoment_.brieven);
				return join(bezwaarBriefJoin, BezwaarBrief_.mergedBrieven, JoinType.LEFT);
			}));
	}

	private static Join<? extends Client, BezwaarMoment> getBezwaarMomentJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.laatstVoltooideBezwaarMoment, JoinType.LEFT);
	}

	@Override
	protected Class<Client> getEntityClass()
	{
		return Client.class;
	}
}
