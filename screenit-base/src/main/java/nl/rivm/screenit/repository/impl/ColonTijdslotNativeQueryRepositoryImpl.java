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

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

import nl.rivm.screenit.repository.colon.ColonTijdslotNativeQueryRepository;

import org.springframework.stereotype.Repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Tuple;

@Repository
public class ColonTijdslotNativeQueryRepositoryImpl implements ColonTijdslotNativeQueryRepository
{
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<Tuple> searchTijdslots(Long intakelocatieId, LocalDateTime startDatum, LocalDateTime eindDatum, LocalTime startTijd, LocalTime eindTijd, Long kamerId,
		List<Integer> dagen, String typeTijdslot)
	{
		var sql = """
			with afspraakslots as
			         (select t.id,
			                 t.vanaf                   as startDatum,
			                 t.tot                     as eindDatum,
			                 k.naam                    as kamer,
			                 k.id                      as kamerId,
			                 cast(t.vanaf as time)     as startTijd,
			                 cast(t.tot as time)       as eindTijd,
			                 extract(DOW from t.vanaf) as dag
			          from colon.tijdslot t
			                   inner join colon.intakekamer k on t.kamer = k.id
			                   inner join algemeen.org_organisatie il on il.id = k.intakelocatie
			          where il.id = :intakelocatieId
			            and il.actief = true
			            and k.actief = true
			            %s
			            and vanaf >= :startDatum
			            and vanaf < :eindDatum
			            and t.type = :typeTijdslot)
			select id as tijdslotId, startDatum, eindDatum, kamerId, kamer
			from afspraakslots
			where eindTijd > :startTijd
			  and startTijd < :eindTijd
			  and dag in (:dagen)
			order by startDatum, kamerId;
			"""
			.formatted(kamerId != null ? "and k.id = :kamerId" : "");

		var query = entityManager.createNativeQuery(sql, Tuple.class);

		query.setParameter("intakelocatieId", intakelocatieId)
			.setParameter("startDatum", startDatum)
			.setParameter("eindDatum", eindDatum)
			.setParameter("typeTijdslot", typeTijdslot)
			.setParameter("startTijd", startTijd)
			.setParameter("eindTijd", eindTijd)
			.setParameter("dagen", dagen);

		if (kamerId != null)
		{
			query.setParameter("kamerId", kamerId);
		}

		return query.getResultList();
	}
}
