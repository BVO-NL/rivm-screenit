package nl.rivm.screenit.batch.jobs.mamma.kansberekening.gemiddelden;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractKansberekeningTasklet;
import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaKansberekeningConstants;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class MammaGemiddeldenTasklet extends MammaAbstractKansberekeningTasklet
{
	private final ICurrentDateSupplier dateSupplier;

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	protected void execute()
	{
		try
		{
			var nu = dateSupplier.getLocalDateTime().toString();

			var sql = IOUtils.toString(MammaGemiddeldenTasklet.class.getResourceAsStream("/updateDeelnameGemiddelden.sql"));
			var sqlQuery = entityManager.createNativeQuery(sql);
			sqlQuery.setParameter("nu", nu);
			sqlQuery.setParameter("deelnamekansberekening_na_weken", Constants.DEELNAMEKANSBEREKENING_NA_WEKEN);

			List<Object[]> list = sqlQuery.getResultList();
			for (Object[] result : list)
			{
				context.putLong(MammaKansberekeningConstants.REGIO_DEELNAME_GEMIDDELDEN_KEY, (Long) result[0]);
				context.putLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_DEELNAME_GEMIDDELDEN_KEY, (Long) result[1]);
			}

			sql = IOUtils.toString(MammaGemiddeldenTasklet.class.getResourceAsStream("/updateOpkomstGemiddelden.sql"));
			sqlQuery = entityManager.createNativeQuery(sql);
			sqlQuery.setParameter("nu", nu);

			list = sqlQuery.getResultList();
			for (Object[] result : list)
			{
				context.putLong(MammaKansberekeningConstants.REGIO_OPKOMST_GEMIDDELDEN__KEY, (Long) result[0]);
				context.putLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_OPKOMST_GEMIDDELDEN_KEY, (Long) result[1]);
			}

		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
