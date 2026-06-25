package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.stereotype.Component;

@Component
@StepScope
public class ClientSelectieItemReader extends AbstractClientSelectieReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	public ClientSelectieItemReader(ICurrentDateSupplier currentDateSupplier)
	{
		setFetchSize(50);
		this.currentDateSupplier = currentDateSupplier;
	}

	@Override
	protected void openInternal(ExecutionContext executionContext) throws ItemStreamException
	{
		cursor = new ClientSelectieItemCursor(entityManager, fetchSize, executionContext, uitgenodigdeClientIds, fitService, currentDateSupplier.getLocalDate(),
			clientRepository);
	}

	@Override
	public ClientCategorieEntry read()
	{
		return getNextEntry();
	}
}
