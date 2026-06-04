package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.ClientGebeurtenisType;

@Getter
public class ClientGebeurtenis implements Serializable, Comparable<ClientGebeurtenis>
{
	@Setter
	private Date datum;

	@Setter
	private ClientGebeurtenisType type;

	private String[] extraParam = new String[0];

	public void setExtraParam(String... extraParam)
	{
		this.extraParam = extraParam;
	}

	@Override
	public int compareTo(ClientGebeurtenis o)
	{
		int result = 0;
		if (o.datum.before(datum))
		{
			result = -1;
		}
		else if (o.datum.after(datum))
		{
			result = 1;
		}
		return result;
	}

}
