package nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodecoordstep;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.CoordinatenService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class PostcodeCoordinatenWriter implements ItemWriter<String>
{

	private final HibernateService hibernateService;

	private final CoordinatenService coordinatenService;

	@Override
	public void write(Chunk<? extends String> chunk)
	{
		for (String item : chunk.getItems())
		{
			String[] lineParts = item.split(",");
			if (lineParts.length == 11 && !lineParts[0].equals("nummeraanduiding_id"))
			{
				String postcode = lineParts[2] + lineParts[3];
				String huisnr = lineParts[4];
				String huisnummerToevoeging = lineParts[5];
				String lat = lineParts[9];
				String lon = lineParts[10];
				coordinatenService.addOrUpdateCoordinaten(postcode, huisnr, huisnummerToevoeging, lat, lon);
			}
		}
		hibernateService.getHibernateSession().flush();
		hibernateService.getHibernateSession().clear();
		LOG.info("alive");
	}
}
