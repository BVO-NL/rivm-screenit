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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieSearchDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Adres_;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Locatie_;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieCriteriaRepository;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Repository
public class LocatieCriteriaRepositoryImpl extends BaseCustomRepositoryImpl implements LocatieCriteriaRepository
{

	@Override
	public List<Locatie> getLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{

		var cb = getCriteriaBuilder();
		var q = cb.createQuery(Locatie.class);

		var r = q.from(Locatie.class);
		var adresJoin = r.join(Locatie_.locatieAdres, JoinType.LEFT);
		var woonplaatsJoin = adresJoin.join(Adres_.woonplaats, JoinType.LEFT);

		q.select(r);

		whereLocaties(q, r, huisarts, locatieSearchDto);

		var resultOptions = locatieSearchDto.getResultOptions();
		if (resultOptions.getSortOptions() != null && !resultOptions.getSortOptions().isEmpty())
		{
			var entry = resultOptions.getSortOptions().entrySet().iterator().next();
			From orderByObject = r;
			var filter = StringUtils.remove(entry.getKey(), '.'); 
			if (StringUtils.startsWith(filter, "locatieAdres"))
			{
				filter = filter.replace("locatieAdres", "");
				orderByObject = adresJoin;
				if (StringUtils.startsWith(filter, "woonplaats"))
				{
					filter = filter.split("woonplaats")[1];
					orderByObject = woonplaatsJoin;
				}
			}

			if (entry.getValue().equalsIgnoreCase("desc"))
			{
				q.orderBy(cb.desc(orderByObject.get(filter)));
			}
			else
			{
				q.orderBy(cb.asc(orderByObject.get(filter)));
			}
		}

		if (resultOptions.getCount() > -1 && resultOptions.getFirst() > -1)
		{
			return getResultList(q, resultOptions.getFirst(), resultOptions.getCount());
		}

		return getResultList(q);
	}

	public void whereLocaties(CriteriaQuery<?> q, Root<Locatie> r, Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{
		var cb = getCriteriaBuilder();
		var statussen = new ArrayList<CervixLocatieStatus>();
		var statusString = locatieSearchDto.getStatus();
		if (statusString != null)
		{
			var locatieStatus = CervixLocatieStatus.valueOf(statusString);
			statussen.add(locatieStatus);

			if (locatieStatus == CervixLocatieStatus.ACTIEF)
			{
				statussen.add(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
			}
		}
		q.where(getHuisartsMetLocatiesMetStatussen(huisarts, EnumSet.copyOf(statussen), cb, r));
	}

	@Override
	public long countLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{
		var cb = getCriteriaBuilder();
		var q = cb.createQuery(Long.class);

		var locatieRoot = q.from(Locatie.class);
		q.select(cb.count(locatieRoot));
		whereLocaties(q, locatieRoot, huisarts, locatieSearchDto);

		return getEntityManager().createQuery(q).getSingleResult();
	}

	public List<Locatie> findByHuisartsAndStatussen(Huisarts huisarts, EnumSet<CervixLocatieStatus> statussen)
	{
		var cb = getCriteriaBuilder();
		var q = cb.createQuery(Locatie.class);
		var r = q.from(Locatie.class);

		q.select(r).where(getHuisartsMetLocatiesMetStatussen(huisarts, statussen, cb, r));

		return getResultList(q);

	}

	private static Predicate getHuisartsMetLocatiesMetStatussen(Huisarts huisarts, EnumSet<CervixLocatieStatus> statussen, CriteriaBuilder cb, Root<Locatie> r)
	{
		var huisartsCriteria = cb.equal(r.get(Locatie_.huisarts), huisarts);
		if (CollectionUtils.isEmpty(statussen))
		{
			return huisartsCriteria;
		}
		return cb.and(huisartsCriteria, r.get(Locatie_.status).in(statussen));
	}
}
