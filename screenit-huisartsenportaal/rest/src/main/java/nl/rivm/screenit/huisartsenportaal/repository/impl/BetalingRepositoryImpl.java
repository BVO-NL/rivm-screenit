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

import java.math.BigDecimal;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.huisartsenportaal.dto.BetalingFilterDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.model.Betaling;
import nl.rivm.screenit.huisartsenportaal.model.Betaling_;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Locatie_;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting_;
import nl.rivm.screenit.huisartsenportaal.repository.BetalingCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Transactional
@Repository
public class BetalingRepositoryImpl extends BaseCustomRepositoryImpl<Betaling> implements BetalingCriteriaRepository
{

	@Override
	public List<Betaling> getBetalingen(Huisarts huisarts, BetalingZoekObjectDto betalingZoekObjectDto)
	{
		TableResultOptionsDto resultOptions = betalingZoekObjectDto.getResultOptions();
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Betaling> query = cb.createQuery(Betaling.class);
		Root<Betaling> betalingRoot = query.from(Betaling.class);
		Join<Betaling, Verrichting> verrichtingJoin = betalingRoot.join(Betaling_.verrichting, JoinType.LEFT);
		Join<Verrichting, Locatie> locatieJoin = verrichtingJoin.join(Verrichting_.huisartsLocatie, JoinType.LEFT);
		query.select(betalingRoot);

		whereBetalingen(query, betalingRoot, huisarts, betalingZoekObjectDto);

		if (resultOptions.getSortOptions() != null && !resultOptions.getSortOptions().isEmpty())
		{
			Map.Entry<String, String> entry = resultOptions.getSortOptions().entrySet().iterator().next();
			From orderByObject = betalingRoot;
			String filter = StringUtils.remove(entry.getKey(), '.'); 
			if (StringUtils.startsWith(filter, "huisartsLocatie"))
			{
				filter = filter.replace("huisartsLocatie", "");
				orderByObject = locatieJoin;
			}
			else if (StringUtils.startsWith(filter, "clientNaam") ||
				StringUtils.startsWith(filter, "monsterId") ||
				StringUtils.startsWith(filter, "regio") ||
				StringUtils.startsWith(filter, "verrichtingsDatum"))
			{
				orderByObject = verrichtingJoin;
			}
			if (entry.getValue().equalsIgnoreCase("desc"))
			{
				query.orderBy(cb.desc(orderByObject.get(filter)));
			}
			else
			{
				query.orderBy(cb.asc(orderByObject.get(filter)));
			}
		}

		if (resultOptions.getCount() > -1 && resultOptions.getFirst() > -1)
		{
			return getResultList(query, resultOptions.getFirst(), resultOptions.getCount());
		}

		return getResultList(query);
	}

	@Override
	public BigDecimal getBetalingenTotaalBedrag(Huisarts huisarts, BetalingZoekObjectDto betalingZoekObjectDto)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<BigDecimal> query = cb.createQuery(BigDecimal.class);

		Root<Betaling> betalingRoot = query.from(Betaling.class);
		query.select(cb.sum(betalingRoot.get(Betaling_.bedrag)));
		whereBetalingen(query, betalingRoot, huisarts, betalingZoekObjectDto);

		return getEntityManager().createQuery(query).getSingleResult();
	}

	@Override
	public long countBetalingen(Huisarts huisarts, BetalingZoekObjectDto betalingZoekObjectDto)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);

		Root<Betaling> betalingRoot = query.from(Betaling.class);
		query.select(cb.count(betalingRoot));
		whereBetalingen(query, betalingRoot, huisarts, betalingZoekObjectDto);

		return getEntityManager().createQuery(query).getSingleResult();
	}

	private CriteriaQuery<?> whereBetalingen(CriteriaQuery<?> query, Root<Betaling> betalingRoot, Huisarts huisarts, BetalingZoekObjectDto zoekObject)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		List<Predicate> condities = new ArrayList<>();
		Join<Betaling, Verrichting> verrichtingJoin = betalingRoot.join(Betaling_.verrichting);
		condities.add(cb.equal(verrichtingJoin.get(Verrichting_.huisarts), huisarts));

		BetalingFilterDto filterDto = zoekObject.getBetalingenZoekObject();
		if (filterDto != null)
		{
			if (filterDto.getLocatie() != null)
			{
				Join<Verrichting, Locatie> locatiesJoin = verrichtingJoin.join(Verrichting_.huisartsLocatie);
				condities.add(cb.equal(locatiesJoin.get(Locatie_.huisartsportaalId), filterDto.getLocatie().getHuisartsportaalId()));
			}
			if (StringUtils.isNotEmpty(filterDto.getClientNaam()))
			{
				condities.add(cb.like(cb.lower(verrichtingJoin.get(Verrichting_.clientNaam)), "%" + StringUtils.lowerCase(filterDto.getClientNaam()) + "%"));
			}
			if (filterDto.getBetalingsdatumVanaf() != null)
			{
				condities.add(cb.greaterThanOrEqualTo(betalingRoot.get(Betaling_.betalingsdatum), filterDto.getBetalingsdatumVanaf()));
			}
			if (filterDto.getBetalingsdatumTotenMet() != null)
			{
				condities.add(
					cb.lessThanOrEqualTo(betalingRoot.get(Betaling_.betalingsdatum), DateUtil.plusTijdseenheid(filterDto.getBetalingsdatumTotenMet(), 1, ChronoUnit.DAYS)));
			}
			if (StringUtils.isNotEmpty(filterDto.getBetalingskenmerk()))
			{
				condities.add(cb.like(cb.lower(betalingRoot.get(Betaling_.betalingsKenmerk)), "%" + StringUtils.lowerCase(filterDto.getBetalingskenmerk()) + "%"));
			}
			if (filterDto.isAlleenZonderBetalingskenmerk())
			{
				condities.add(cb.isNull(betalingRoot.get(Betaling_.betalingsKenmerk)));
			}
		}
		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}
		return query;
	}
}
