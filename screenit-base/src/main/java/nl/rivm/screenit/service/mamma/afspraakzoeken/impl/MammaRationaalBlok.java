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
import java.util.Comparator;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;

import static nl.rivm.screenit.util.BigDecimalUtil.isPositive;

@Slf4j
class MammaRationaalBlok extends MammaRationaal
{
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final MammaAfspraakOptieZoekContext zoekContext;

	private final List<MammaRationaalAfspraakOptie> afspraakOptiesInBlok = new ArrayList<>();

	private final boolean heeftInitieelVrijeCapaciteit;

	@Getter
	private BigDecimal gebruikteCapaciteit = BigDecimal.ZERO;

	MammaRationaalBlok(MammaCapaciteitBlokDto capaciteitBlokDto, MammaAfspraakOptieZoekContext zoekContext)
	{
		this.capaciteitBlokDto = capaciteitBlokDto;
		this.zoekContext = zoekContext;

		LOG.debug("{} afspraken in capaciteitBlok {} vanaf {}, beschikbaar: {}, vrij: {}", capaciteitBlokDto.getAfspraakDtos().size(),
			capaciteitBlokDto.getId(), capaciteitBlokDto.getVanaf(), capaciteitBlokDto.getBeschikbareCapaciteit(), capaciteitBlokDto.getVrijeCapaciteit());

		capaciteitBlokDto.getAfspraakDtos().forEach(bestaandeAfspraak ->
		{
			var afspraakOptie = MammaRationaalAfspraakOptie.vanBestaandeAfspraak(bestaandeAfspraak, zoekContext);
			addAfspraakOptie(afspraakOptie);
			gebruikteCapaciteit = gebruikteCapaciteit.add(bestaandeAfspraak.getBenodigdeCapaciteit());
		});

		heeftInitieelVrijeCapaciteit = isPositive(capaciteitBlokDto.getBeschikbareCapaciteit().subtract(gebruikteCapaciteit));
	}

	MammaRationaalAfspraakOptie getAfspraakOptie()
	{
		LOG.debug("getAfspraakOptie op blok vanaf: {}, beschikbaar: {}, gebruikt: {}, ratio: {}, blokId: {} ",
			capaciteitBlokDto.getVanaf(), capaciteitBlokDto.getBeschikbareCapaciteit(), gebruikteCapaciteit, getRatioTekst(), capaciteitBlokDto.getId());

		MammaRationaalAfspraakOptie afspraakOptie;
		var capaciteitBlokVanaf = capaciteitBlokDto.getVanaf();
		if (afspraakOptiesInBlok.isEmpty())
		{

			afspraakOptie = MammaRationaalAfspraakOptie.voorNieuweOptie(capaciteitBlokVanaf.toLocalTime(), capaciteitBlokDto.getTot(), capaciteitBlokDto, zoekContext);
		}
		else
		{
			var eersteAfspraakOptieVanaf = afspraakOptiesInBlok.get(0).getVanaf();
			if (!eersteAfspraakOptieVanaf.equals(capaciteitBlokVanaf.toLocalTime()))
			{

				afspraakOptie = MammaRationaalAfspraakOptie.voorNieuweOptie(capaciteitBlokVanaf.toLocalTime(), eersteAfspraakOptieVanaf, capaciteitBlokDto, zoekContext);
			}
			else
			{
				afspraakOptie = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(afspraakOptiesInBlok)
					.splitsNieuweAfspraakOptie(zoekContext);
			}
		}

		if (!heeftInitieelVrijeCapaciteit)
		{
			afspraakOptie.setGeldigeAfspraak(false);
		}

		gebruikteCapaciteit = gebruikteCapaciteit.add(afspraakOptie.getBenodigdeCapaciteit());
		return addAfspraakOptie(afspraakOptie);
	}

	private MammaRationaalAfspraakOptie addAfspraakOptie(MammaRationaalAfspraakOptie afspraakOptie)
	{
		afspraakOptiesInBlok.add(afspraakOptie);
		afspraakOptiesInBlok.sort(Comparator.comparing(MammaAfspraakOptie::getVanaf));
		return afspraakOptie;
	}

	BigDecimal getBeschikbareCapaciteit()
	{
		return capaciteitBlokDto.getBeschikbareCapaciteit();
	}

	@Override
	BigDecimal getTeller()
	{
		return gebruikteCapaciteit;
	}

	@Override
	BigDecimal getNoemer()
	{
		return getBeschikbareCapaciteit();
	}
}
