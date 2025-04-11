package nl.rivm.screenit.model.enums;

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

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public enum OnderzoeksresultatenActieType
{
	ONDERZOEKSRESULTATEN_VERWIJDERD_BMHK("Onderzoeksresultaten voor baarmoederhalskanker zijn verwijderd"),
	ONDERZOEKSRESULTATEN_VERWIJDERD_BK("Onderzoeksresultaten voor borstkanker zijn verwijderd"),
	ONDERZOEKSRESULTATEN_VERWIJDERD_DK("Onderzoeksresultaten voor darmkanker zijn verwijderd");

	private final String omschrijving;

	public static OnderzoeksresultatenActieType getTypeVoorBevolkingsonderzoek(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		return switch (bevolkingsonderzoek)
		{
			case COLON -> ONDERZOEKSRESULTATEN_VERWIJDERD_DK;
			case MAMMA -> ONDERZOEKSRESULTATEN_VERWIJDERD_BK;
			case CERVIX -> ONDERZOEKSRESULTATEN_VERWIJDERD_BMHK;
		};
	}
}
