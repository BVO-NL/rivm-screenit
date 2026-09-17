package nl.rivm.screenit.batch.jobs.generalis.coordinaten.gemeentecoordstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.WoonplaatsService;

import org.springframework.batch.infrastructure.item.Chunk;
import org.springframework.batch.infrastructure.item.ItemWriter;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class GemeenteCoordinatenWriter implements ItemWriter<String>
{
	@PersistenceContext
	private EntityManager entityManager;

	private final CoordinatenService coordinatenService;

	private final WoonplaatsService woonplaatsService;

	@Override
	public void write(Chunk<? extends String> chunk)
	{
		for (var item : chunk.getItems())
		{
			var lineParts = item.split(",");

			if (lineParts.length >= 10 && !lineParts[0].equals("pcnl_plaatsid"))
			{

				var plaatscode = lineParts[2];
				var gemcode = lineParts[3];
				var woonplaats = lineParts[4];
				var latitude = lineParts[8];
				var longitude = lineParts[9];
				coordinatenService.updateGemeenteCoordinaten(gemcode, latitude, longitude);
				woonplaatsService.saveOrUpdateWoonplaats(plaatscode, woonplaats, gemcode);
			}
		}
		entityManager.flush();
		entityManager.clear();
	}
}
