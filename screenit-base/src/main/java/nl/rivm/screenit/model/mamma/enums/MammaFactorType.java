package nl.rivm.screenit.model.mamma.enums;

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

import java.math.BigDecimal;

import nl.rivm.screenit.model.ScreeningOrganisatie;

public enum MammaFactorType
{
	GEEN,
	EERSTE_ONDERZOEK,
	DUBBELE_TIJD,
	MINDERVALIDE;

	public BigDecimal getFactor(ScreeningOrganisatie screeningOrganisatie)
	{
		return switch (this)
		{
			case GEEN -> BigDecimal.ONE;
			case EERSTE_ONDERZOEK -> screeningOrganisatie.getFactorEersteOnderzoekBk();
			case DUBBELE_TIJD -> screeningOrganisatie.getFactorDubbeleTijdBk();
			case MINDERVALIDE -> screeningOrganisatie.getFactorMindervalideBk();
			default -> throw new IllegalStateException("Unexpected value: " + this);
		};
	}

	public static MammaFactorType getFactorType(boolean isTehuisClient, MammaDoelgroep doelgroep, boolean eersteOnderzoek)
	{
		if (isTehuisClient)
		{
			return doelgroep.equals(MammaDoelgroep.MINDERVALIDE) ? MINDERVALIDE : DUBBELE_TIJD;
		}

		return switch (doelgroep)
		{
			case REGULIER -> eersteOnderzoek ? EERSTE_ONDERZOEK : GEEN;
			case DUBBELE_TIJD -> DUBBELE_TIJD;
			case MINDERVALIDE -> MINDERVALIDE;
			default -> throw new IllegalStateException("Unexpected value: " + doelgroep);
		};
	}
}
