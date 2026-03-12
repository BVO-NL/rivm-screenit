package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;

import nl.rivm.screenit.service.impl.BaseBestandVerwerkingContext;
import nl.topicuszorg.util.collections.CollectionUtils;

public class MedewerkersBestandVerwerkingContext extends BaseBestandVerwerkingContext
{
	private static final String MEDEWERKERCODE = "medewerkercode";

	private int medewerkercodeColumn = -1;

	public MedewerkersBestandVerwerkingContext(File file) throws Exception
	{
		super(file, 1);
		bepaalVolgordeHeaders();

	}

	@Override
	public void bepaalVolgordeHeaders()
	{
		var headers = getHuidigeRegel();
		if (CollectionUtils.isNotEmpty(headers))
		{
			for (String header : headers)
			{
				String geformatteerdeHeader = header.toLowerCase().trim();
				if (MEDEWERKERCODE.equals(geformatteerdeHeader))
				{
					medewerkercodeColumn = headers.indexOf(header);
				}

			}
			if (medewerkercodeColumn == -1)
			{
				throw new IllegalStateException("Geen header met medewerkercode gevonden in het bestand.");
			}
		}
	}

	public String getMedewerkercodeVanHuidigeRegel() throws IllegalStateException
	{
		String value = huidigeLine[medewerkercodeColumn];
		if (value != null)
		{
			return value.trim();
		}
		else
		{
			return null;
		}
	}
}
