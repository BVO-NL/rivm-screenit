package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.repository.colon.ColoscopieCentrumColonCapaciteitVerdelingRepository;
import nl.rivm.screenit.specification.colon.ColoscopieCentrumColonCapaciteitVerdelingSpecification;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service("ColoscopieCentrumColonCapaciteitVerdelingDataProviderService")
public class ColoscopieCentrumColonCapaciteitVerdelingDataProviderServiceImpl extends RepositoryDataProviderService<ColoscopieCentrumColonCapaciteitVerdeling, ColoscopieCentrumColonCapaciteitVerdelingRepository, ColonIntakelocatie>
{
	@Override
	protected Specification<ColoscopieCentrumColonCapaciteitVerdeling> getSpecification(ColonIntakelocatie filter, Sort sortParam)
	{
		return ColoscopieCentrumColonCapaciteitVerdelingSpecification.heeftIntakelocatie(filter);
	}
}
