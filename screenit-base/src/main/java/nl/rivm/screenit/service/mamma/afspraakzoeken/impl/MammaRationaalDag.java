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
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;

import static nl.rivm.screenit.util.BigDecimalUtil.isNegative;

class MammaRationaalDag extends MammaRationaal
{
	private final List<MammaRationaalBlok> rationaalBlokken = new ArrayList<>();

	private final boolean genoegDagcapaciteitVoorMindervalide;

	@Getter
	private final BigDecimal aflopendDagNummer;

	@Getter
	private BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;

	private BigDecimal gebruikteCapaciteit = BigDecimal.ZERO;

	@Setter
	private BigDecimal streefcapaciteit;

	MammaRationaalDag(List<MammaCapaciteitBlokDto> capaciteitBlokDtos, BigDecimal aflopendDagNummer, MammaAfspraakOptieZoekContext zoekContext)
	{
		this.aflopendDagNummer = aflopendDagNummer;

		capaciteitBlokDtos.forEach(capaciteitBlok ->
		{
			var rationaalBlok = new MammaRationaalBlok(capaciteitBlok, zoekContext);
			rationaalBlokken.add(rationaalBlok);
			beschikbareCapaciteit = beschikbareCapaciteit.add(rationaalBlok.getBeschikbareCapaciteit());
			gebruikteCapaciteit = gebruikteCapaciteit.add(rationaalBlok.getGebruikteCapaciteit());
		});

		streefcapaciteit = beschikbareCapaciteit;
		genoegDagcapaciteitVoorMindervalide = beschikbareCapaciteit.compareTo(zoekContext.getMinimaleDagCapaciteitMindervalideAfspraken()) >= 0;
	}

	MammaRationaalAfspraakOptie getAfspraakOptie()
	{
		var rationaalBlok = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(rationaalBlokken);

		var afspraakOptie = rationaalBlok.getAfspraakOptie();
		if (afspraakOptie.isMindervalide() && !genoegDagcapaciteitVoorMindervalide)
		{
			afspraakOptie.setGeldigeAfspraak(false);
		}

		gebruikteCapaciteit = gebruikteCapaciteit.add(afspraakOptie.getBenodigdeCapaciteit());

		return afspraakOptie;
	}

	BigDecimal getVrijeCapaciteit()
	{
		return beschikbareCapaciteit.subtract(gebruikteCapaciteit);
	}

	boolean vrijeCapaciteitNietNegatief()
	{
		return !isNegative(getVrijeCapaciteit());
	}

	@Override
	BigDecimal getTeller()
	{
		return gebruikteCapaciteit;
	}

	@Override
	BigDecimal getNoemer()
	{
		return streefcapaciteit;
	}
}
