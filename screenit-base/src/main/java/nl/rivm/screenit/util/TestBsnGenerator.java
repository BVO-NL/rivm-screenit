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

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.topicuszorg.util.bsn.BsnUtils;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class TestBsnGenerator
{
	private static final Set<String> generatedBsns = ConcurrentHashMap.newKeySet();

	public static String getValideBsn()
	{
		while (true)
		{
			var candidate = generateValidBsn();
			if (generatedBsns.add(candidate))
			{
				return candidate;
			}
		}
	}

	private static String generateValidBsn()
	{
		while (true)
		{
			var number = ThreadLocalRandom.current().nextInt(100_000_000, 1_000_000_000);
			var bsn = String.format("%09d", number);

			if (BsnUtils.isValidBSN(bsn))
			{
				return bsn;
			}
		}
	}

}
