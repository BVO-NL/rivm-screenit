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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.impl.BaseBestandVerwerkingContext;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang.StringUtils;

@Slf4j
public class ClientenBestandVerwerkingContext extends BaseBestandVerwerkingContext
{

	private static final String BSN = "bsn";

	private static final String GEBOORTEDATUM = "geboortedatum";

	private int bsnColumn = -1;

	private int geboortedatumColumn = -1;

	public ClientenBestandVerwerkingContext(File file) throws Exception
	{
		super(file, 2);
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
				if (BSN.equals(geformatteerdeHeader))
				{
					bsnColumn = headers.indexOf(header);
				}
				else if (GEBOORTEDATUM.equals(geformatteerdeHeader))
				{
					geboortedatumColumn = headers.indexOf(header);
				}
			}
			if (bsnColumn == -1 || geboortedatumColumn == -1)
			{
				throw new IllegalStateException("Geen header met bsn of geboortedatum gevonden in het bestand.");
			}
		}
	}

	public Date getGeboortedatumVanHuidigeRegel() throws IllegalStateException
	{
		String value = huidigeLine[geboortedatumColumn];
		if (StringUtils.isNotBlank(value))
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			try
			{
				return format.parse(value);
			}
			catch (ParseException e)
			{
				try
				{
					format = new SimpleDateFormat("d-M-yyyy");
					return format.parse(value);
				}
				catch (ParseException e1)
				{
					throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
				}
			}
		}
		throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
	}

	public String getBsnVanHuidigeRegel()
	{
		return huidigeLine[bsnColumn].trim();
	}

}
