package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

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
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.util.mamma.MammaMindervalideUtil;

import static nl.rivm.screenit.util.BigDecimalUtil.isPositive;

@Slf4j
class MammaRationaalBlok extends MammaRationaal
{
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final MammaAfspraakOptieZoekContext zoekContext;

	private final List<MammaRationaalAfspraak> rationaalAfsprakenInBlok = new ArrayList<>();

	private final boolean heeftInitieelVrijeCapaciteit;

	@Getter
	private BigDecimal gebruikteCapaciteit = BigDecimal.ZERO;

	MammaRationaalBlok(MammaCapaciteitBlokDto capaciteitBlokDto, MammaAfspraakOptieZoekContext zoekContext)
	{
		this.capaciteitBlokDto = capaciteitBlokDto;
		this.zoekContext = zoekContext;
		voegMindervalideReserveringenToeAlsAfsprakenAanCapaciteitBlok(capaciteitBlokDto);

		capaciteitBlokDto.getAfspraakDtos().forEach(bestaandeAfspraak ->
		{
			var afspraakOptie = MammaRationaalAfspraak.vanBestaandeAfspraak(bestaandeAfspraak, zoekContext);
			addAfspraakOptie(afspraakOptie);
			gebruikteCapaciteit = gebruikteCapaciteit.add(bestaandeAfspraak.getBenodigdeCapaciteit());
		});

		LOG.debug("{} afspraken in capaciteitBlok {} vanaf {}, beschikbaar: {}, gebruikteCapaciteit: {}, Mv-Reserveringen:{}",
			capaciteitBlokDto.getAfspraakDtos().size(), capaciteitBlokDto.getId(), capaciteitBlokDto.getVanaf(), capaciteitBlokDto.getBeschikbareCapaciteit(), gebruikteCapaciteit,
			capaciteitBlokDto.getMindervalideReserveringen().size());
		heeftInitieelVrijeCapaciteit = isPositive(capaciteitBlokDto.getBeschikbareCapaciteit().subtract(gebruikteCapaciteit));
	}

	private void voegMindervalideReserveringenToeAlsAfsprakenAanCapaciteitBlok(MammaCapaciteitBlokDto capaciteitBlokDto)
	{
		if (!MammaMindervalideUtil.zijnMindervalideReserveringenVrijgegeven(capaciteitBlokDto, zoekContext.getVrijgevenMindervalideReserveringenTotEnMetDatum()))
		{
			var onbezetteMindervalideReserveringAfspraken = capaciteitBlokDto.getMindervalideReserveringen().stream()
				.filter(mvReserveringVanaf -> MammaMindervalideUtil.isMindervalideReserveringOnbezet(capaciteitBlokDto, mvReserveringVanaf))
				.map(mvReserveringVanaf ->
					maakMindervalideAfspraakVoorMindervalideReservering(capaciteitBlokDto, mvReserveringVanaf)
				).toList();

			capaciteitBlokDto.getAfspraakDtos().addAll(onbezetteMindervalideReserveringAfspraken);
		}

		sorteerAfsprakenOpVanafTijdEnZetAfspraakTot(capaciteitBlokDto);
	}

	private static void sorteerAfsprakenOpVanafTijdEnZetAfspraakTot(MammaCapaciteitBlokDto capaciteitBlokDto)
	{
		capaciteitBlokDto.getAfspraakDtos().sort(Comparator.comparing(MammaAfspraakDto::getVanaf));
		MammaAfspraakDto vorigeAfspraakDto = null;
		for (var afspraakDto : capaciteitBlokDto.getAfspraakDtos())
		{
			if (vorigeAfspraakDto != null)
			{
				vorigeAfspraakDto.setTot(afspraakDto.getVanaf().toLocalTime());
			}
			vorigeAfspraakDto = afspraakDto;
		}
		if (vorigeAfspraakDto != null)
		{
			vorigeAfspraakDto.setTot(capaciteitBlokDto.getTot());
		}
	}

	private MammaAfspraakDto maakMindervalideAfspraakVoorMindervalideReservering(MammaCapaciteitBlokDto capaciteitBlokDto, LocalTime reserveringVanaf)
	{
		var afspraakVanaf = capaciteitBlokDto.getDatum().atTime(reserveringVanaf);
		var afspraakTot = reserveringVanaf.plusMinutes(zoekContext.getBenodigdeMinutenVoorMindervalideAfspraak());
		return new MammaAfspraakDto(capaciteitBlokDto, afspraakVanaf, afspraakTot, zoekContext.getBenodigdeCapaciteitPerMindervalideAfspraak(), true, false);
	}

	MammaRationaalAfspraak getAfspraakOptie()
	{
		LOG.debug("getAfspraakOptie op blok vanaf: {}, beschikbaar: {}, gebruikt: {}, ratio: {}, blokId: {} ",
			capaciteitBlokDto.getVanaf(), capaciteitBlokDto.getBeschikbareCapaciteit(), gebruikteCapaciteit, getRatioTekst(), capaciteitBlokDto.getId());

		MammaRationaalAfspraak afspraakOptie;
		var capaciteitBlokVanaf = capaciteitBlokDto.getVanaf();
		if (rationaalAfsprakenInBlok.isEmpty())
		{

			afspraakOptie = MammaRationaalAfspraak.voorNieuweOptie(capaciteitBlokVanaf.toLocalTime(), capaciteitBlokDto.getTot(), capaciteitBlokDto, zoekContext);
		}
		else
		{
			var eersteAfspraakOptieVanaf = rationaalAfsprakenInBlok.getFirst().getVanaf();
			if (!eersteAfspraakOptieVanaf.equals(capaciteitBlokVanaf.toLocalTime()))
			{

				afspraakOptie = MammaRationaalAfspraak.voorNieuweOptie(capaciteitBlokVanaf.toLocalTime(), eersteAfspraakOptieVanaf, capaciteitBlokDto, zoekContext);
			}
			else
			{
				afspraakOptie = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(rationaalAfsprakenInBlok)
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

	private MammaRationaalAfspraak addAfspraakOptie(MammaRationaalAfspraak afspraakOptie)
	{
		rationaalAfsprakenInBlok.add(afspraakOptie);
		rationaalAfsprakenInBlok.sort(Comparator.comparing(MammaRationaalAfspraak::getVanaf));
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
