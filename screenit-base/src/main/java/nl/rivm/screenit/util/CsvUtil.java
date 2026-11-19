package nl.rivm.screenit.util;

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
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Reader;
import java.io.Writer;
import java.util.List;

import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.CSVWriter;
import com.opencsv.exceptions.CsvException;
import com.opencsv.exceptions.CsvValidationException;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CsvUtil
{
	public static CSVParser maakParser(char scheidingsteken)
	{
		return new CSVParserBuilder().withSeparator(scheidingsteken).build();
	}

	public static CSVReader maakReader(Reader reader, char scheidingsteken)
	{
		return new CSVReaderBuilder(reader)
			.withCSVParser(maakParser(scheidingsteken))
			.build();
	}

	public static CSVWriter maakWriter(Writer writer, char scheidingsteken)
	{
		return maakWriter(writer, scheidingsteken, CSVWriter.DEFAULT_QUOTE_CHARACTER);
	}

	public static CSVWriter maakWriter(Writer writer, char scheidingsteken, char quoteteken)
	{
		return new CSVWriter(writer, scheidingsteken, quoteteken, CSVWriter.DEFAULT_ESCAPE_CHARACTER, CSVWriter.DEFAULT_LINE_END);
	}

	public static String[] leesRegel(CSVReader reader) throws IOException
	{
		try
		{
			return reader.readNext();
		}
		catch (CsvValidationException e)
		{
			throw new IOException(e);
		}
	}

	public static List<String[]> leesAlleRegels(CSVReader reader) throws IOException
	{
		try
		{
			return reader.readAll();
		}
		catch (CsvException e)
		{
			throw new IOException(e);
		}
	}

	public static void truncateLastLine(File file)
	{
		try (var randomAccessFile = new RandomAccessFile(file, "rw"))
		{
			randomAccessFile.setLength(randomAccessFile.length() - 1);
		}
		catch (IOException e)
		{
			LOG.error("Error bij het truncaten van de laatste regel van het bestand", e);
		}
	}
}
