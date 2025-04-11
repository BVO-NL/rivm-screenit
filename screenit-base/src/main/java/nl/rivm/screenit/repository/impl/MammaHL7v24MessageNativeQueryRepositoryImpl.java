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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.repository.mamma.MammaHL7v24MessageNativeQueryRepository;

import org.springframework.stereotype.Repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

@Repository
@Slf4j
public class MammaHL7v24MessageNativeQueryRepositoryImpl implements MammaHL7v24MessageNativeQueryRepository
{
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public void verwijderAlleBerichtenVanClient(long clientId)
	{
		var sql = """
			delete from mamma.hl7v24_message h7v24m where h7v24m.dto_json like ('%\\"clientId\\":' || :clientId || '%')
			""";

		var query = entityManager.createNativeQuery(sql)
			.setParameter("clientId", clientId);

		var result = query.getFirstResult();

		LOG.info("{} berichten verwijderd voor client {}", result, clientId);
	}

	@Override
	public void verwijderAlleBerichtenExclusiefImsVoorClient(long clientId, String deleteOrmStatus, String goingToDeleteOrm)
	{
		var sql = """
			delete from mamma.hl7v24_message h7v24m 
			where h7v24m.dto_json like ('%\\"clientId\\":' || :clientId || '%')
				and h7v24m.dto_json not like ('%\\"status\\":\\"' || :deleteOrmStatus || '\\"%') 
				and h7v24m.dto_json not like ('%\\"status\\":\\"' || :goingToDeleteOrm || '\\"%')
			""";

		var query = entityManager.createNativeQuery(sql)
			.setParameter("clientId", clientId)
			.setParameter("deleteOrmStatus", deleteOrmStatus)
			.setParameter("goingToDeleteOrm", goingToDeleteOrm);

		var result = query.getFirstResult();

		LOG.info("{} berichten verwijderd exclusief Ims voor client {}", result, clientId);
	}
}
