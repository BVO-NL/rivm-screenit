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
import java.util.Collection;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakReserveringDto;
import nl.rivm.screenit.repository.mamma.MammaAfspraakReserveringNativeQueryRepository;

import org.springframework.stereotype.Repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

@Repository
public class MammaAfspraakReserveringNativeQueryRepositoryImpl implements MammaAfspraakReserveringNativeQueryRepository
{

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<MammaAfspraakReserveringDto> haalReserveringenOpVoorCapaciteitsblokken(LocalDateTime ophaalMoment, Collection<Long> capaciteitBlokIds, Long clientId)
	{
		var sql = """ 
			select ar.capaciteit_blok as capaciteitBlokId, ar.vanaf, ar.opkomstkans, d.doelgroep, d.eerste_onderzoek as eersteOnderzoek, t.id as tehuisId
			from mamma.afspraak_reservering ar
			join gedeeld.pat_patient pp on pp.id = ar.client
			join mamma.dossier d on pp.mamma_dossier = d.id
			left join mamma.tehuis t on d.tehuis = t.id
			where ar.aangemaakt_op > :ophaalMoment
			and ar.capaciteit_blok in :capaciteitBlokIds
			""";

		if (clientId != null)
		{
			sql += "and ar.client <> :clientId";
		}

		var query = entityManager.createNativeQuery(sql, "MammaAfspraakReserveringDtoMapping")
			.setParameter("ophaalMoment", ophaalMoment)
			.setParameter("capaciteitBlokIds", capaciteitBlokIds);

		if (clientId != null)
		{
			query.setParameter("clientId", clientId);
		}

		return (List<MammaAfspraakReserveringDto>) query.getResultList();
	}
}
