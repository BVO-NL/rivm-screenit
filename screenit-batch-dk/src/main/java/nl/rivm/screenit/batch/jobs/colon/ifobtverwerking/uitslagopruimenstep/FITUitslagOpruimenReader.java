package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.uitslagopruimenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.colon.ColonFITUitslagSpecification.heeftAnalyseDatumTussen;

@Component
@RequiredArgsConstructor
public class FITUitslagOpruimenReader extends BaseSpecificationScrollableResultReader<IFOBTUitslag>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<IFOBTUitslag> createSpecification()
	{
		return heeftAnalyseDatumTussen(Range.lessThan(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusMonths(4))));
	}
}
