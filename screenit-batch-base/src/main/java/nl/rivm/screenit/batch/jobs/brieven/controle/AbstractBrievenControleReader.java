package nl.rivm.screenit.batch.jobs.brieven.controle;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.algemeen.MergedBrievenSpecification;

import org.springframework.data.jpa.domain.Specification;

public abstract class AbstractBrievenControleReader<MB extends MergedBrieven<?>> extends BaseSpecificationScrollableResultReader<MB>
{

	@Override
	protected Specification<MB> createSpecification()
	{
		return MergedBrievenSpecification.<MB> isVerwijderd(false)
			.and(MergedBrievenSpecification.isVrijgegeven(false))
			.and(MergedBrievenSpecification.<MB> heeftGeenBriefType()
				.or(MergedBrievenSpecification.heeftBriefTypeIn(BriefType.getBriefTypesMetOrganisatieType(OrganisatieType.SCREENINGSORGANISATIE))));
	}
}
