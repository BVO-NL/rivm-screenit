package nl.rivm.screenit.model.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serial;
import java.io.Serializable;
import java.util.Comparator;

import nl.rivm.screenit.model.Permissie;

public class PermissieComparator implements Comparator<Permissie>, Serializable
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Override
	public int compare(Permissie o1, Permissie o2)
	{
		if (o1.getId() == null && o2.getId() != null)
		{
			return 1;
		}
		if (o1.getId() != null && o2.getId() == null)
		{
			return -1;
		}
		if (o1.getRecht() == null && o2.getRecht() == null)
		{
			return 0;
		}
		if (o1.getRecht() == null)
		{
			return 1;
		}
		if (o2.getRecht() == null)
		{
			return -1;
		}
		return o1.getRecht().getOmschrijving().compareTo(o2.getRecht().getOmschrijving());
	}

}
