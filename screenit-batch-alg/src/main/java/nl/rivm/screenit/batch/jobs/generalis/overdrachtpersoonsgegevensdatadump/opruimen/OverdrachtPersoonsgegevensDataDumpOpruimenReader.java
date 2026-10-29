package nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.opruimen;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.OverdrachtPersoonsgegevensSpecification.heeftNogTeVerwijderenDataDump;

@Component
@AllArgsConstructor
public class OverdrachtPersoonsgegevensDataDumpOpruimenReader extends BaseSpecificationScrollableResultReader<OverdrachtPersoonsgegevens>
{
	private static final int BEWAARTERMIJN_DAGEN = 14;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<OverdrachtPersoonsgegevens> createSpecification()
	{
		var grensDatum = currentDateSupplier.getLocalDateTime().minusDays(BEWAARTERMIJN_DAGEN);
		return heeftNogTeVerwijderenDataDump(grensDatum);
	}
}
