package nl.rivm.screenit.util;

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

import java.io.IOException;
import java.util.concurrent.Executors;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class Hl7v2BerichtUtil
{

	public static OUL_R22 createMessage(String bericht)
	{
		try (HapiContext context = new DefaultHapiContext())
		{
			context.setExecutorService(Executors.newSingleThreadExecutor()); 
			var parser = context.getPipeParser();
			return (OUL_R22) parser.parse(bericht);
		}
		catch (HL7Exception | IOException e)
		{
			throw new IllegalStateException("Er ging iets mis met parsen van het bericht", e);
		}
	}
}
