package nl.rivm.screenit.main.service;

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

import java.util.List;

import nl.rivm.screenit.model.AbstractHoudbaarheid;

import org.springframework.data.domain.Sort;

public interface HoudbaarheidService
{
	<H extends AbstractHoudbaarheid> List<H> getHoudbaarheidItems(Class<H> clazz, long first, long count, Sort sort);

	<H extends AbstractHoudbaarheid> Long countHoudbaarheidItems(Class<H> clazz);

	<H extends AbstractHoudbaarheid> boolean overlaptBestaandeReeks(H vervalDatum);
}
