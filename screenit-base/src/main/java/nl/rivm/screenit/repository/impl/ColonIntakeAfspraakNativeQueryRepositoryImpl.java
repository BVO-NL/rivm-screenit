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

import java.time.LocalTime;
import java.util.List;

import nl.rivm.screenit.repository.colon.ColonIntakeAfspraakNativeQueryRepository;

import org.springframework.stereotype.Repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

@Repository
public class ColonIntakeAfspraakNativeQueryRepositoryImpl implements ColonIntakeAfspraakNativeQueryRepository
{
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<Long> countColonIntakeAfsprakenInNacht(LocalTime startTijd, LocalTime eindTijd)
	{
		var sql = """
			with time_table as (
			    select make_time(cast(extract(HOUR from papp.vanaf) as INTEGER), cast(extract(MINUTE from papp.vanaf) as INTEGER), cast (extract(SECOND from papp.vanaf) as DOUBLE PRECISION)) as start_time,
			    make_time(cast(extract(HOUR from papp.tot) as INTEGER), cast(extract(MINUTE from papp.tot) as INTEGER), cast (extract(SECOND from papp.tot) as DOUBLE PRECISION)) as end_time
			    from colon.afspraakslot  afs
			    inner join colon.tijdslot papp on afs.id=papp.id
			    where papp.vanaf > now()
			)
			select COUNT(*)
			from time_table
			where start_time < :startTijd or end_time > :eindTijd
			""";

		var query = entityManager.createNativeQuery(sql)
			.setParameter("startTijd", startTijd)
			.setParameter("eindTijd", eindTijd);

		return List.of(((Number) query.getSingleResult()).longValue());
	}

	@Override
	public List<Long> countColonIntakeAfsprakenOpDag(int dagVanDeWeek)
	{
		var sql = """
			select count(afs.id) as count
			from colon.afspraakslot  afs
			  inner join colon.tijdslot ts on afs.id = ts.id
			where ts.vanaf > now() and DATE_PART('DOW', ts.vanaf) = :dag
			""";

		var query = entityManager.createNativeQuery(sql)
			.setParameter("dag", dagVanDeWeek);

		return List.of(((Number) query.getSingleResult()).longValue());
	}
}
