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

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaRationaal;
import nl.rivm.screenit.util.TimeRange;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

import static nl.rivm.screenit.Constants.BK_TIJDVAK_MIN;
import static nl.rivm.screenit.Constants.BK_TIJDVAK_SEC;

@Slf4j
public class MammaAfspraakOptieImpl extends MammaRationaal implements MammaAfspraakOptie
{
	@Getter
	private final MammaCapaciteitBlokDto capaciteitBlokDto;

	private final MammaScreeningsEenheidDto screeningsEenheidDto;

	private final LocalTime minimaleAfspraakVanaf;

	private final LocalTime maximaleAfspraakTot;

	@Getter
	private final BigDecimal benodigdeCapaciteit;

	@Getter
	private final boolean minderValide;

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

	public MammaAfspraakOptieImpl(MammaCapaciteitBlokDto capaciteitBlok, TimeRange mogelijkeAfspraakPeriode, TimeRange afspraakPeriode, BigDecimal benodigdeCapaciteit,
		MammaScreeningsEenheidDto screeningsEenheid, boolean minderValide, boolean dubbeleTijd)
	{
		capaciteitBlokDto = capaciteitBlok;
		minimaleAfspraakVanaf = mogelijkeAfspraakPeriode.getVanaf();
		maximaleAfspraakTot = mogelijkeAfspraakPeriode.getTot();
		datum = capaciteitBlok.vanaf.toLocalDate();
		vanaf = afspraakPeriode.getVanaf();
		tot = afspraakPeriode.getTot().isAfter(mogelijkeAfspraakPeriode.getTot()) ? mogelijkeAfspraakPeriode.getTot() : afspraakPeriode.getTot(); 
		this.minderValide = minderValide;
		this.dubbeleTijd = dubbeleTijd;
		this.benodigdeCapaciteit = benodigdeCapaciteit;
		screeningsEenheidDto = screeningsEenheid;
		geldigeAfspraak = geldigeDuurEnPeriode(vanaf, tot);
	}

	private boolean geldigeDuurEnPeriode(LocalTime vanaf, LocalTime tot)
	{
		return tot.isAfter(vanaf) 
			&& (!minderValide || isGeldigeDuurVoorMindervalideAfspraak(vanaf, tot))
			&& (!dubbeleTijd || isGeldigeDuurVoorDubbeleTijdAfspraak(vanaf, tot));
	}

	private BigDecimal getDuurInSeconden()
	{
		return BigDecimal.valueOf(Duration.between(vanaf, tot).getSeconds());
	}

	public MammaAfspraakOptieImpl splitsNieuweAfspraakOptie(BigDecimal nieuweBenodigdeCapaciteit, BigDecimal nieuweFactor,
		boolean nieuweOptieIsMinderValide,
		boolean nieuweOptieIsDubbeleTijd)
	{
		MammaAfspraakOptieImpl nieuweOptie;

		if (!getDuurInSeconden().equals(BK_TIJDVAK_SEC))
		{
			var nieuweOnafgerondeVanaf = nieuwVanafNaSplitsingNaarVerhoudingBenodigdeCapaciteit(nieuweBenodigdeCapaciteit);
			var maximaleBegintijdInBlok = maximaleBegintijdInBlok(nieuweFactor);
			if (maximaleBegintijdInBlok.isBefore(nieuweOnafgerondeVanaf))
			{
				nieuweOptie = maakNieuweOptie(maximaleBegintijdInBlok, nieuweBenodigdeCapaciteit, nieuweOptieIsMinderValide, nieuweOptieIsDubbeleTijd);
			}
			else
			{
				nieuweOptie = maakNieuweOptieInPassendTijdvak(nieuweOnafgerondeVanaf, nieuweBenodigdeCapaciteit, nieuweOptieIsMinderValide,
					nieuweOptieIsDubbeleTijd);
			}
		}
		else
		{
			nieuweOptie = maakNieuweOptieInKleinstMogelijkeTijdvak(nieuweBenodigdeCapaciteit, nieuweOptieIsMinderValide, nieuweOptieIsDubbeleTijd);
		}

		LOG.debug(
			"AfspraakOptie {}-{} MV={}, DT={}, geldig={}, afgesplitsteOngeldig={} gesplitst naar {}-{}, MV={}, DT={}, geldig={}, afgesplitsteOngeldig={}, capaciteitBlokId={}",
			vanaf, tot, minderValide, dubbeleTijd, geldigeAfspraak, afgesplitsteOptiesZijnOngeldig, nieuweOptie.vanaf, nieuweOptie.tot, nieuweOptie.minderValide,
			nieuweOptie.dubbeleTijd, nieuweOptie.geldigeAfspraak, nieuweOptie.afgesplitsteOptiesZijnOngeldig, capaciteitBlokDto.id);

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
		var maximaleBegintijdInBlok = maximaleAfspraakTot.minusMinutes(MammaPlanningUtil.minimumTijdvak(factorNieuweOptie));
		if (maximaleBegintijdInBlok.isBefore(minimaleAfspraakVanaf))
		{
			maximaleBegintijdInBlok = minimaleAfspraakVanaf;
		}
		return maximaleBegintijdInBlok;
	}

	private MammaAfspraakOptieImpl maakNieuweOptie(LocalTime nieuweVanaf, BigDecimal nieuweBenodigdeCapaciteit, boolean nieuweIsMinderValide, boolean nieuwIsDubbeleTijd)
	{
		var nieuweOptie = new MammaAfspraakOptieImpl(capaciteitBlokDto, TimeRange.of(minimaleAfspraakVanaf, maximaleAfspraakTot),
			TimeRange.of(nieuweVanaf, tot), nieuweBenodigdeCapaciteit, screeningsEenheidDto, nieuweIsMinderValide, nieuwIsDubbeleTijd);
		controleerDatNieuweOptieVoorEindtijdBlokBegint(nieuweOptie);
		controleerDatNieuweMinderValideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(nieuweOptie);
		controleerDatBestaandeAfspraakGeldigBlijft(nieuweOptie);
		return nieuweOptie;
	}

	private void controleerDatNieuweOptieVoorEindtijdBlokBegint(MammaAfspraakOptieImpl nieuweOptie)
	{
		nieuweOptie.geldigeAfspraak &= maximaleAfspraakTot.isAfter(nieuweOptie.vanaf);
	}

	private void controleerDatNieuweMinderValideOfDubbeleTijdOptieNietGelijkValtMetBestaandeAfspraak(MammaAfspraakOptieImpl nieuweOptie)
	{
		if (screeningsEenheidDto.isEnkeleMammograaf() && (nieuweOptie.minderValide || nieuweOptie.dubbeleTijd))
		{
			nieuweOptie.geldigeAfspraak &= nieuweOptie.vanaf.isAfter(vanaf);
		}
	}

	private void controleerDatBestaandeAfspraakGeldigBlijft(MammaAfspraakOptieImpl nieuweOptie)
	{
		nieuweOptie.afgesplitsteOptiesZijnOngeldig = afgesplitsteOptiesZijnOngeldig;
		nieuweOptie.geldigeAfspraak &= !afgesplitsteOptiesZijnOngeldig;

		if (geldigeAfspraak && (minderValide || dubbeleTijd))
		{

			var nieuweOptieHoudtOudeGeldig = !screeningsEenheidDto.isEnkeleMammograaf() || geldigeDuurEnPeriode(vanaf, nieuweOptie.vanaf);
			nieuweOptie.geldigeAfspraak &= nieuweOptieHoudtOudeGeldig;

			nieuweOptie.afgesplitsteOptiesZijnOngeldig |= !nieuweOptieHoudtOudeGeldig;
		}
	}

	private boolean isGeldigeDuurVoorMindervalideAfspraak(LocalTime optieVanaf, LocalTime optieTot)
	{

		return !screeningsEenheidDto.isEnkeleMammograaf()
			|| Duration.between(optieVanaf, optieTot).toMinutes() >= 15;
	}

	private boolean isGeldigeDuurVoorDubbeleTijdAfspraak(LocalTime optieVanaf, LocalTime optieTot)
	{
		return !screeningsEenheidDto.isEnkeleMammograaf()
			|| Duration.between(optieVanaf, optieTot).toMinutes() >= 10;
	}

	private MammaAfspraakOptieImpl maakNieuweOptieInPassendTijdvak(LocalTime nieuweOnafgerondeVanaf, BigDecimal nieuweBenodigdeCapaciteit,
		boolean nieuweIsMinderValide,
		boolean nieuweIsDubbeleTijd)
	{
		var optieVroegeTijdvak = maakNieuweOptie(rondAfNaarBeginTijdvak(nieuweOnafgerondeVanaf), nieuweBenodigdeCapaciteit, nieuweIsMinderValide, nieuweIsDubbeleTijd);
		var optieLateTijdvak = maakNieuweOptie(optieVroegeTijdvak.vanaf.plusMinutes(BK_TIJDVAK_MIN), nieuweBenodigdeCapaciteit, nieuweIsMinderValide,
			nieuweIsDubbeleTijd);
		return kiesBesteOptie(nieuweOnafgerondeVanaf, optieVroegeTijdvak, optieLateTijdvak);
	}

	private static LocalTime rondAfNaarBeginTijdvak(LocalTime nieuweOptieVanaf)
	{
		var minuten = nieuweOptieVanaf.truncatedTo(ChronoUnit.MINUTES).getMinute();
		return nieuweOptieVanaf.withMinute(minuten - minuten % BK_TIJDVAK_MIN).withSecond(0);
	}

	private MammaAfspraakOptieImpl kiesBesteOptie(LocalTime nieuweOnafgerondeVanaf, MammaAfspraakOptieImpl optieVroegeTijdvak, MammaAfspraakOptieImpl optieLateTijdvak)
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

	private MammaAfspraakOptieImpl kiesDichtstbijzijndeOptie(LocalTime onafgerondeVanaf, MammaAfspraakOptieImpl optieVroegeTijdvak, MammaAfspraakOptieImpl optieLateTijdvak)
	{
		return Duration.between(optieVroegeTijdvak.vanaf, onafgerondeVanaf).compareTo(Duration.between(onafgerondeVanaf, optieLateTijdvak.vanaf)) <= 0 ?
			optieVroegeTijdvak : optieLateTijdvak;
	}

	private MammaAfspraakOptieImpl maakNieuweOptieInKleinstMogelijkeTijdvak(BigDecimal nieuweBenodigdeCapaciteit, boolean nieuweOptieIsMinderValide,
		boolean nieuweOptieIsDubbeleTijd)
	{
		var nieuweOptie = maakNieuweOptie(vanaf, nieuweBenodigdeCapaciteit, nieuweOptieIsMinderValide, nieuweOptieIsDubbeleTijd);

		nieuweOptie.geldigeAfspraak &= !nieuweOptieIsMinderValide && !nieuweOptieIsDubbeleTijd;
		return nieuweOptie;
	}

	@Override
	public BigDecimal getDeeltal()
	{
		return benodigdeCapaciteit;
	}

	@Override
	public BigDecimal getDeler()
	{
		return getDuurInSeconden();
	}
}
