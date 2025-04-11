package nl.rivm.screenit.main.service.algemeen.impl;

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
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.repository.algemeen.RolRepository;
import nl.rivm.screenit.specification.algemeen.RolSpecification;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service("RolDataProviderService")
public class RolDataProviderServiceImpl extends RepositoryDataProviderService<Rol, RolRepository, Rol>
{

	@Override
	protected Specification<Rol> getSpecification(Rol filter, Sort sortParam)
	{
		return RolSpecification.filterBevolkingsonderzoek(filter.getBevolkingsonderzoeken()).and(RolSpecification.filterIsActief(filter.getActief()));
	}
}
