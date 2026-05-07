package nl.rivm.screenit.mamma.se.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

public class SemverUtil
{
	public static boolean isVersieGreaterOrEqual(String version, String minVersion)
	{
		if (version == null || minVersion == null)
		{
			throw new IllegalArgumentException("Versie en minimale versie mogen niet null zijn.");
		}

		var versionParts = version.split("\\.");
		var minVersionParts = minVersion.split("\\.");

		var maxLength = Math.max(versionParts.length, minVersionParts.length);
		for (int i = 0; i < maxLength; i++)
		{
			var versionPart = i < versionParts.length ? parseVersieOnderdeel(versionParts[i]) : 0;
			var minVersionPart = i < minVersionParts.length ? parseVersieOnderdeel(minVersionParts[i]) : 0;

			if (versionPart > minVersionPart)
			{
				return true;
			}
			else if (versionPart < minVersionPart)
			{
				return false;
			}
		}
		return true;
	}

	private static int parseVersieOnderdeel(String part)
	{
		try
		{
			return Integer.parseInt(part);
		}
		catch (NumberFormatException e)
		{
			var numericPart = part.replaceAll("[^0-9]", "");
			return numericPart.isEmpty() ? 0 : Integer.parseInt(numericPart);
		}
	}
}
