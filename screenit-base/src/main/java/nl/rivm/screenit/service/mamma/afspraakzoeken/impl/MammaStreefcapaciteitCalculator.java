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

import static java.math.BigDecimal.ONE;
import static java.math.BigDecimal.ZERO;
import static nl.rivm.screenit.util.BigDecimalUtil.isNegative;
import static nl.rivm.screenit.util.BigDecimalUtil.isPositive;
import static nl.rivm.screenit.util.BigDecimalUtil.isZero;

class MammaStreefcapaciteitCalculator
{
	private final List<MammaRationaalDag> aflopendeDagen;

	MammaStreefcapaciteitCalculator(List<MammaRationaalDag> rationaalDagen)
	{
		this.aflopendeDagen = rationaalDagen.stream()
			.filter(this::isAflopendeDagVoorStreefcapaciteit)
			.toList();
	}

	private boolean isAflopendeDagVoorStreefcapaciteit(MammaRationaalDag dag)
	{
		return isPositive(dag.getAflopendDagNummer());
	}

	void updateStreefCapaciteit()
	{
		var gewogenBeschikbareCapaciteit = ZERO;
		var beschikbareCapaciteit = ZERO;
		var vrijeCapaciteit = ZERO;

		for (var dag : aflopendeDagen)
		{
			beschikbareCapaciteit = beschikbareCapaciteit.add(dag.getBeschikbareCapaciteit());
			gewogenBeschikbareCapaciteit = gewogenBeschikbareCapaciteit.add(dag.getAflopendDagNummer().multiply(dag.getBeschikbareCapaciteit()));
			vrijeCapaciteit = vrijeCapaciteit.add(dag.getVrijeCapaciteit());
		}

		if (isNegative(vrijeCapaciteit))
		{
			vrijeCapaciteit = ZERO;
		}

		var aflopendPerDagFactor = berekenAflopendPerDagFactor(beschikbareCapaciteit, gewogenBeschikbareCapaciteit, vrijeCapaciteit);

		aflopendeDagen.forEach(dag -> updateStreefcapaciteitVoorDag(dag, aflopendPerDagFactor));
	}

	private BigDecimal berekenAflopendPerDagFactor(BigDecimal beschikbareCapaciteit, BigDecimal gewogenBeschikbareCapaciteit, BigDecimal vrijeCapaciteit)
	{
		if (isZero(beschikbareCapaciteit))
		{
			return ONE; 
		}

		var gewogenGemiddeldeDag = divide(gewogenBeschikbareCapaciteit, beschikbareCapaciteit);

		if (isZero(gewogenGemiddeldeDag))
		{
			return ONE; 
		}

		var factorVrijeCapaciteit = divide(vrijeCapaciteit, beschikbareCapaciteit);
		return divide(factorVrijeCapaciteit, gewogenGemiddeldeDag);
	}

	private void updateStreefcapaciteitVoorDag(MammaRationaalDag dag, BigDecimal aflopendPerDag)
	{
		var streeffactor = ONE.subtract(aflopendPerDag.multiply(dag.getAflopendDagNummer()));

		if (!isPositive(streeffactor))
		{
			streeffactor = new BigDecimal("0.0001"); 
		}

		dag.setStreefcapaciteit(dag.getBeschikbareCapaciteit().multiply(streeffactor));
	}

	private BigDecimal divide(BigDecimal teller, BigDecimal noemer)
	{
		return teller.divide(noemer, 10, RoundingMode.HALF_UP);
	}
}
