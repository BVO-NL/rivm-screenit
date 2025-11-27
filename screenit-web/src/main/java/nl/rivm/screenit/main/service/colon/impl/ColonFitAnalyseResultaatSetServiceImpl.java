package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import jakarta.persistence.criteria.Predicate;

import nl.rivm.screenit.main.model.colon.ColonHoudbaarheidFitReeksFilter;
import nl.rivm.screenit.main.service.colon.ColonFitAnalyseResultaatSetService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat_;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RangeUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSetSpecification.filterLaboratorium;
import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSetSpecification.filterStatus;
import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSetSpecification.heeftStatusDatumTussen;
import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSpecification.heeftAnalyseDatumTussen;

@Service
public class ColonFitAnalyseResultaatSetServiceImpl implements ColonFitAnalyseResultaatSetService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonFitAnalyseResultaatSetRepository fitAnalyseResultaatSetRepository;

	@Override
	public List<ColonFitAnalyseResultaatSet> getResultaatSets(ColonHoudbaarheidFitReeksFilter filter, long first, long count, Sort sort)
	{
		if (isRangeInvalide(filter))
		{
			return List.of();
		}

		return fitAnalyseResultaatSetRepository.findWith(getAnalyseResultaatSetSpecification(filter), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countResultaatSets(ColonHoudbaarheidFitReeksFilter filter)
	{
		if (isRangeInvalide(filter))
		{
			return 0;
		}

		return fitAnalyseResultaatSetRepository.count(getAnalyseResultaatSetSpecification(filter));
	}

	private Specification<ColonFitAnalyseResultaatSet> getAnalyseResultaatSetSpecification(ColonHoudbaarheidFitReeksFilter filter)
	{
		return (r, q, cb) ->
		{

			var predicates = new ArrayList<Predicate>();
			predicates.add(filterStatus(filter.getStatus()).and(filterLaboratorium(filter.getLab())).toPredicate(r, q, cb));
			var datumVan = filter.getDatumVan() != null ? DateUtil.startDag(filter.getDatumVan()) : null;
			var datumTot = filter.getDatumTot() != null ? DateUtil.plusDagen(DateUtil.startDag(filter.getDatumTot()), 1) : null;

			var range = RangeUtil.range(datumVan, BoundType.CLOSED, datumTot, BoundType.OPEN);
			if (filter.isAnalyseDatum())
			{
				var subquery = q.subquery(Long.class);
				var subRoot = subquery.from(ColonFitAnalyseResultaat.class);
				subquery.select(subRoot.get(ColonFitAnalyseResultaat_.analyseResultaatSet).get(AbstractHibernateObject_.id))
					.where(heeftAnalyseDatumTussen(range).toPredicate(subRoot, q, cb));
				predicates.add(cb.in(r.get(AbstractHibernateObject_.id)).value(subquery));

			}
			else
			{
				predicates.add(heeftStatusDatumTussen(range).toPredicate(r, q, cb));
			}
			return composePredicates(cb, predicates);
		};
	}

	private boolean isRangeInvalide(ColonHoudbaarheidFitReeksFilter filter)
	{
		return filter.getDatumVan() != null && filter.getDatumTot() != null && filter.getDatumVan().after(filter.getDatumTot());
	}

	@Override
	@Transactional
	public void verwijderResultaatSets(List<ColonFitAnalyseResultaatSet> resultaatSets, Account ingelogdAccount)
	{
		ColonFitLaboratorium lab = null;
		for (var resultaatSet : resultaatSets)
		{
			resultaatSet.setStatus(ColonFitAnalyseResultaatSetStatus.VERWIJDERD);
			lab = resultaatSet.getLaboratorium();
			hibernateService.saveOrUpdate(resultaatSet);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTAAT_SET_VERWIJDERD, ingelogdAccount, "Labid qbase " + lab.getQbasenummer(),
				Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	@Transactional
	public void autoriseerResultaatSets(List<ColonFitAnalyseResultaatSet> resultaatSets, Account ingelogdAccount)
	{
		ColonFitLaboratorium lab = null;
		for (var resultaatSet : resultaatSets)
		{
			lab = resultaatSet.getLaboratorium();
			resultaatSet.setStatus(ColonFitAnalyseResultaatSetStatus.GEAUTORISEERD);
			hibernateService.saveOrUpdate(resultaatSet);
		}
		if (lab != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTAAT_SET_GEAUTORISEERD, ingelogdAccount, "Labid qbase " + lab.getQbasenummer(),
				Bevolkingsonderzoek.COLON);
		}
	}
}
