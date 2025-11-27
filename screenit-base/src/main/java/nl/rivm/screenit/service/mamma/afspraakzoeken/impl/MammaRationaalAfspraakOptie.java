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
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.Constants.BK_TIJDVAK_MIN;
import static nl.rivm.screenit.Constants.BK_TIJDVAK_SEC;

@Slf4j
class MammaRationaalAfspraakOptie extends MammaRationaal implements MammaAfspraakOptie
{
	private static final int BENODIGDE_MINUTEN_VOOR_DUBBELE_TIJD_AFSPRAAK = 10;

	@Getter
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final boolean enkeleMammograaf;

	private final int benodigdeMinutenVoorMindervalideAfspraak;

	private final LocalTime begintijdBlok;

	private final LocalTime eindtijdBlok;

	@Getter
	private final BigDecimal benodigdeCapaciteit;

	@Getter
	private final boolean mindervalide;

	private final boolean dubbeleTijd;

	@Getter
	private final LocalDate datum;

	@Getter
	private final LocalTime vanaf;

	private LocalTime tot;

	@Getter
	@Setter
	private boolean geldigeAfspraak;

	private boolean afgesplitsteOptiesZijnOngeldig;

	private MammaRationaalAfspraakOptie(MammaCapaciteitBlokDto capaciteitBlok, Range<LocalTime> afspraakPeriode, BigDecimal benodigdeCapaciteit,
		boolean mindervalide, boolean dubbeleTijd, MammaAfspraakOptieZoekContext zoekContext)
	{
		capaciteitBlokDto = capaciteitBlok;
		begintijdBlok = capaciteitBlok.getVanaf().toLocalTime();
		eindtijdBlok = capaciteitBlok.getTot();
		datum = capaciteitBlok.getVanaf().toLocalDate();
		vanaf = afspraakPeriode.lowerEndpoint();
		tot = afspraakPeriode.upperEndpoint();
		this.mindervalide = mindervalide;
		this.dubbeleTijd = dubbeleTijd;
		this.benodigdeCapaciteit = benodigdeCapaciteit;
		enkeleMammograaf = zoekContext.isEnkeleMammograaf();
		benodigdeMinutenVoorMindervalideAfspraak = zoekContext.getBenodigdeMinutenVoorMindervalideAfspraak();
		geldigeAfspraak = geldigeDuurEnPeriode(vanaf, tot);
	}

	static MammaRationaalAfspraakOptie vanBestaandeAfspraak(MammaAfspraakDto bestaandeAfspraak, MammaAfspraakOptieZoekContext zoekContext)
	{
		var afspraakOptie = new MammaRationaalAfspraakOptie(bestaandeAfspraak.getCapaciteitBlokDto(),
			Range.closedOpen(bestaandeAfspraak.getVanaf().toLocalTime(), bestaandeAfspraak.getTot()),
			bestaandeAfspraak.getBenodigdeCapaciteit(), bestaandeAfspraak.isMindervalide(), bestaandeAfspraak.isDubbeleTijd(), zoekContext);
		afspraakOptie.setGeldigeAfspraak(true);
		return afspraakOptie;
	}

	static MammaRationaalAfspraakOptie voorNieuweOptie(LocalTime vanaf, LocalTime tot, MammaCapaciteitBlokDto capaciteitBlok, MammaAfspraakOptieZoekContext zoekContext)
	{
		return new MammaRationaalAfspraakOptie(capaciteitBlok, Range.closedOpen(vanaf, tot),
			zoekContext.getBenodigdeCapaciteit(), zoekContext.isMindervalide(), zoekContext.isDubbeleTijd(), zoekContext);
	}

	private boolean geldigeDuurEnPeriode(LocalTime vanaf, LocalTime tot)
	{
		return tot.isAfter(vanaf) 
			&& (!mindervalide || isGeldigeDuurVoorMindervalideAfspraak(vanaf, tot))
			&& (!dubbeleTijd || isGeldigeDuurVoorDubbeleTijdAfspraak(vanaf, tot));
	}

	private BigDecimal getDuurInSeconden()
	{
		return BigDecimal.valueOf(Duration.between(vanaf, tot).getSeconds());
	}

	MammaRationaalAfspraakOptie splitsNieuweAfspraakOptie(MammaAfspraakOptieZoekContext zoekContext)
	{
		MammaRationaalAfspraakOptie nieuweOptie;

		if (!getDuurInSeconden().equals(BK_TIJDVAK_SEC))
		{
			var nieuweOnafgerondeVanaf = nieuwVanafNaSplitsingNaarVerhoudingBenodigdeCapaciteit(zoekContext.getBenodigdeCapaciteit());
			var maximaleBegintijdInBlok = maximaleBegintijdInBlok(zoekContext.getFactor());
			if (maximaleBegintijdInBlok.isBefore(nieuweOnafgerondeVanaf))
			{
				nieuweOptie = maakNieuweOptie(maximaleBegintijdInBlok, zoekContext);
			}
			else
			{
				nieuweOptie = maakNieuweOptieInPassendTijdvak(nieuweOnafgerondeVanaf, zoekContext);
			}
		}
		else
		{
			nieuweOptie = maakNieuweOptieInKleinstMogelijkeTijdvak(zoekContext);
		}

		LOG.debug(
			"AfspraakOptie {}-{} MV={}, DT={}, geldig={}, afgesplitsteOngeldig={} gesplitst naar {}-{}, MV={}, DT={}, geldig={}, afgesplitsteOngeldig={}, capaciteitBlokId={}",
			vanaf, tot, mindervalide, dubbeleTijd, geldigeAfspraak, afgesplitsteOptiesZijnOngeldig, nieuweOptie.vanaf, nieuweOptie.tot, nieuweOptie.mindervalide,
			nieuweOptie.dubbeleTijd, nieuweOptie.geldigeAfspraak, nieuweOptie.afgesplitsteOptiesZijnOngeldig, capaciteitBlokDto.getId());

		tot = nieuweOptie.vanaf;
		return nieuweOptie;
	}

	private LocalTime nieuwVanafNaSplitsingNaarVerhoudingBenodigdeCapaciteit(BigDecimal nieuweOptieBenodigdeCapaciteit)
	{
		var somBenodigdeCapaciteit = benodigdeCapaciteit.add(nieuweOptieBenodigdeCapaciteit);
		var vermenigvuldigingsfactor = benodigdeCapaciteit.divide(somBenodigdeCapaciteit, 5, RoundingMode.HALF_UP);
		var nieuweDuurInSeconden = getDuurInSeconden().multiply(vermenigvuldigingsfactor);
		return vanaf.plusSeconds(nieuweDuurInSeconden.longValue());
	}

	private LocalTime maximaleBegintijdInBlok(BigDecimal factorNieuweOptie)
	{
		var maximaleBegintijdInBlok = eindtijdBlok.minusMinutes(MammaPlanningUtil.minimumTijdvak(factorNieuweOptie));
		if (maximaleBegintijdInBlok.isBefore(begintijdBlok))
		{
			maximaleBegintijdInBlok = begintijdBlok;
		}
		return maximaleBegintijdInBlok;
	}

	private MammaRationaalAfspraakOptie maakNieuweOptie(LocalTime nieuweVanaf, MammaAfspraakOptieZoekContext zoekContext)
	{
		var nieuweOptie = voorNieuweOptie(nieuweVanaf, tot, capaciteitBlokDto, zoekContext);
		controleerDatNieuweOptieVoorEindtijdBlokBegint(nieuweOptie);
		controleerDatNieuweMindervalideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(nieuweOptie);
		controleerDatBestaandeAfspraakGeldigBlijft(nieuweOptie);
		return nieuweOptie;
	}

	private void controleerDatNieuweOptieVoorEindtijdBlokBegint(MammaRationaalAfspraakOptie nieuweOptie)
	{
		nieuweOptie.geldigeAfspraak &= eindtijdBlok.isAfter(nieuweOptie.vanaf); 
	}

	private void controleerDatNieuweMindervalideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(MammaRationaalAfspraakOptie nieuweOptie)
	{
		if (enkeleMammograaf && (nieuweOptie.mindervalide || nieuweOptie.dubbeleTijd))
		{
			nieuweOptie.geldigeAfspraak &= nieuweOptie.vanaf.isAfter(vanaf);
		}
	}

	private void controleerDatBestaandeAfspraakGeldigBlijft(MammaRationaalAfspraakOptie nieuweOptie)
	{
		nieuweOptie.afgesplitsteOptiesZijnOngeldig = afgesplitsteOptiesZijnOngeldig;
		nieuweOptie.geldigeAfspraak &= !afgesplitsteOptiesZijnOngeldig;

		if (geldigeAfspraak && (mindervalide || dubbeleTijd))
		{

			var nieuweOptieHoudtOudeGeldig = !enkeleMammograaf || geldigeDuurEnPeriode(vanaf, nieuweOptie.vanaf);
			nieuweOptie.geldigeAfspraak &= nieuweOptieHoudtOudeGeldig;

			nieuweOptie.afgesplitsteOptiesZijnOngeldig |= !nieuweOptieHoudtOudeGeldig;
		}
	}

	private boolean isGeldigeDuurVoorMindervalideAfspraak(LocalTime optieVanaf, LocalTime optieTot)
	{
		return !enkeleMammograaf
			|| Duration.between(optieVanaf, optieTot).toMinutes() >= benodigdeMinutenVoorMindervalideAfspraak;
	}

	private boolean isGeldigeDuurVoorDubbeleTijdAfspraak(LocalTime optieVanaf, LocalTime optieTot)
	{
		return !enkeleMammograaf
			|| Duration.between(optieVanaf, optieTot).toMinutes() >= BENODIGDE_MINUTEN_VOOR_DUBBELE_TIJD_AFSPRAAK;
	}

	private MammaRationaalAfspraakOptie maakNieuweOptieInPassendTijdvak(LocalTime nieuweOnafgerondeVanaf, MammaAfspraakOptieZoekContext zoekContext)
	{
		var optieVroegeTijdvak = maakNieuweOptie(rondAfNaarBeginTijdvak(nieuweOnafgerondeVanaf), zoekContext);
		var optieLateTijdvak = maakNieuweOptie(optieVroegeTijdvak.vanaf.plusMinutes(BK_TIJDVAK_MIN), zoekContext);
		return kiesBesteOptie(nieuweOnafgerondeVanaf, optieVroegeTijdvak, optieLateTijdvak);
	}

	private static LocalTime rondAfNaarBeginTijdvak(LocalTime nieuweOptieVanaf)
	{
		var minuten = nieuweOptieVanaf.truncatedTo(ChronoUnit.MINUTES).getMinute();
		return nieuweOptieVanaf.withMinute(minuten - minuten % BK_TIJDVAK_MIN).withSecond(0);
	}

	private MammaRationaalAfspraakOptie kiesBesteOptie(LocalTime nieuweOnafgerondeVanaf, MammaRationaalAfspraakOptie optieVroegeTijdvak,
		MammaRationaalAfspraakOptie optieLateTijdvak)
	{
		if (optieVroegeTijdvak.geldigeAfspraak && !optieLateTijdvak.geldigeAfspraak)
		{
			return optieVroegeTijdvak;
		}
		else if (!optieVroegeTijdvak.geldigeAfspraak && optieLateTijdvak.geldigeAfspraak)
		{
			return optieLateTijdvak;
		}
		return kiesDichtstbijzijndeOptie(nieuweOnafgerondeVanaf, optieVroegeTijdvak, optieLateTijdvak);
	}

	private MammaRationaalAfspraakOptie kiesDichtstbijzijndeOptie(LocalTime onafgerondeVanaf, MammaRationaalAfspraakOptie optieVroegeTijdvak,
		MammaRationaalAfspraakOptie optieLateTijdvak)
	{
		return Duration.between(optieVroegeTijdvak.vanaf, onafgerondeVanaf).compareTo(Duration.between(onafgerondeVanaf, optieLateTijdvak.vanaf)) <= 0 ?
			optieVroegeTijdvak : optieLateTijdvak;
	}

	private MammaRationaalAfspraakOptie maakNieuweOptieInKleinstMogelijkeTijdvak(MammaAfspraakOptieZoekContext zoekContext)
	{
		var nieuweOptie = maakNieuweOptie(vanaf, zoekContext);

		nieuweOptie.geldigeAfspraak &= !zoekContext.isMindervalide() && !zoekContext.isDubbeleTijd();
		return nieuweOptie;
	}

	@Override
	BigDecimal getTeller()
	{
		return benodigdeCapaciteit;
	}

	@Override
	BigDecimal getNoemer()
	{
		return getDuurInSeconden();
	}
}
