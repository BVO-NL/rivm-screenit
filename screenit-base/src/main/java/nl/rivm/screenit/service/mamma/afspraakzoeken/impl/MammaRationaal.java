package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import static nl.rivm.screenit.util.BigDecimalUtil.isZero;

class MammaRationaal implements Comparable<MammaRationaal>
{
	private final BigDecimal teller;

	private final BigDecimal noemer;

	MammaRationaal(BigDecimal teller, BigDecimal noemer)
	{
		this.teller = teller;
		this.noemer = noemer;
	}

	MammaRationaal()
	{
		teller = null;
		noemer = null;
	}

	static MammaRationaal gecombineerdeRationaal(List<? extends MammaRationaal> rationalen)
	{
		var totaalTeller = BigDecimal.ZERO;
		var totaalNoemer = BigDecimal.ZERO;
		for (var rationaal : rationalen)
		{
			totaalTeller = totaalTeller.add(rationaal.getTeller());
			totaalNoemer = totaalNoemer.add(rationaal.getNoemer());
		}
		return new MammaRationaal(totaalTeller, totaalNoemer);
	}

	BigDecimal getTeller()
	{
		return teller;
	}

	BigDecimal getNoemer()
	{
		return noemer;
	}

	BigDecimal getRatio()
	{
		return getTeller().divide(getNoemer(), 10, RoundingMode.HALF_UP);
	}

	String getRatioTekst()
	{
		return noemerIsZero() ? "oneindig" : getRatio().toString();
	}

	private boolean noemerIsZero()
	{
		return isZero(getNoemer());
	}

	@Override
	public int compareTo(MammaRationaal other)
	{
		var thisZeroNoemer = noemerIsZero();
		var otherZeroNoemer = other.noemerIsZero();

		if (thisZeroNoemer)
		{
			if (otherZeroNoemer)
			{
				return 0;
			}
			else
			{
				return 1;
			}
		}
		else
		{
			if (otherZeroNoemer)
			{
				return -1;
			}
			else
			{
				return this.getRatio().compareTo(other.getRatio());
			}
		}
	}
}
