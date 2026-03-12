package nl.rivm.screenit.service.impl;

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

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import au.com.bytecode.opencsv.CSVReader;

@Slf4j
public abstract class BaseBestandVerwerkingContext implements AutoCloseable
{
	protected CSVReader reader = null;

	@Getter
	protected int regelnummer;

	protected String[] huidigeLine = null;

	public BaseBestandVerwerkingContext(File file, int aantalKolommen) throws Exception
	{
		getMeestBruikbareReader(file, aantalKolommen);
		regelnummer = 1;
	}

	protected void getMeestBruikbareReader(File file, int aantalKolommen) throws Exception
	{
		CSVReader testReader = null;
		try
		{
			testReader = new CSVReader(new FileReader(file), ',');
			huidigeLine = testReader.readNext();
			if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= aantalKolommen)
			{
				reader = testReader;
			}
			else
			{
				testReader.close();
				huidigeLine = null;
				testReader = new CSVReader(new FileReader(file), ';');
				huidigeLine = testReader.readNext();
				if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= aantalKolommen)
				{
					reader = testReader;
				}
				else
				{
					testReader.close();
					testReader = null;
					throw new IllegalStateException("File die is aangeleverd voldoet niet aan het juiste formaat");
				}
			}
		}
		catch (Exception e)
		{
			if (testReader != null)
			{
				try
				{

					testReader.close();
				}
				catch (IOException e1)
				{
					LOG.error("Van het projectbestand kan de reader niet worden gesloten", e);
					throw e;
				}
			}
		}
	}

	public List<String> getHuidigeRegel()
	{
		return Arrays.asList(huidigeLine);
	}

	public boolean isErEenNieuweRegel() throws IOException
	{
		huidigeLine = reader.readNext();
		if (huidigeLine != null)
		{
			regelnummer++;
			return true;
		}
		return false;
	}

	public void close()
	{
		if (reader != null)
		{
			try
			{
				reader.close();
			}
			catch (IOException e)
			{
				LOG.error("Fout bij sluiten van reader", e);
			}
		}
	}

	public abstract void bepaalVolgordeHeaders();
}
