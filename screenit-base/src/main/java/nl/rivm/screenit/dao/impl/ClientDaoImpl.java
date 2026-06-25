package nl.rivm.screenit.dao.impl;

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

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.model.Client;

import org.springframework.stereotype.Repository;

@Repository
public class ClientDaoImpl implements ClientDao
{

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public boolean heeftDossierMetRondeOfAfmelding(Client client)
	{
		var query = entityManager.createNativeQuery("SELECT 1"
			+ " FROM gedeeld.client client"
			+ "  LEFT JOIN colon.dossier co ON client.colon_dossier = co.id"
			+ "  LEFT JOIN cervix.dossier ce ON client.cervix_dossier = ce.id"
			+ "  LEFT JOIN mamma.dossier ma ON client.mamma_dossier = ma.id"
			+ " WHERE client.id = :clientId"
			+ "  AND (co.laatste_screening_ronde IS NOT NULL OR co.laatste_afmelding IS NOT NULL"
			+ "   OR ce.laatste_screening_ronde IS NOT NULL OR ce.laatste_afmelding IS NOT NULL"
			+ "   OR ma.laatste_screening_ronde IS NOT NULL OR ma.laatste_afmelding IS NOT NULL);");
		query.setParameter("clientId", client.getId());
		return !query.getResultList().isEmpty();
	}
}
