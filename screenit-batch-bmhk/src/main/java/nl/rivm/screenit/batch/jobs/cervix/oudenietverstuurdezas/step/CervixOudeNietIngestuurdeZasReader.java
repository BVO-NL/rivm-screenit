package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.Charset;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.CervixOudeNietIngestuurdeZasConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseSqlScrollableResultReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.service.OrganisatieParameterService;

import org.apache.commons.io.IOUtils;
import org.hibernate.HibernateException;
import org.hibernate.ScrollableResults;
import org.hibernate.query.NativeQuery;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CervixOudeNietIngestuurdeZasReader extends BaseSqlScrollableResultReader
{
	private final OrganisatieParameterService organisatieParameterService;

	private Integer getMaxAantalClienten()
	{
		return organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_OUDE_ZAS);
	}

	private Long getProjectId()
	{
		var context = getExecutionContext();
		if (!context.containsKey(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID))
		{
			return context.getLong(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID);
		}
		throw new IllegalStateException();
	}

	@Override
	protected NativeQuery createNativeQuery() throws HibernateException
	{
		try
		{
			var maxAantalClienten = getMaxAantalClienten();
			if (maxAantalClienten <= 0)
			{
				return null;
			}
			var sql = IOUtils.toString(getClass().getResourceAsStream("CervixOudeNietIngestuurdeZasReader.sql"), Charset.defaultCharset());
			var query = getHibernateSession().createNativeQuery(sql);
			query.setParameter("projectId", getProjectId());
			query.setMaxResults(maxAantalClienten);
			return query;
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	protected Long getScrollableResult(ScrollableResults scrollableResults)
	{
		return ((BigInteger) scrollableResults.get()[0]).longValue();
	}
}
