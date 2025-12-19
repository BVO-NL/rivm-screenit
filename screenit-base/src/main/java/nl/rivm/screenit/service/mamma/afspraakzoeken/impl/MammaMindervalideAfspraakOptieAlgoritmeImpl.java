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
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaBaseAfspraakOptieDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptieAlgoritme;
import nl.rivm.screenit.util.RangeUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil;

import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.util.DateUtil.toUtilDate;
import static nl.rivm.screenit.util.mamma.MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak;
import static nl.rivm.screenit.util.mamma.MammaPlanningUtil.isEnkeleMammograaf;

@Component("mammaMindervalideAfspraakOptieAlgoritme")
@Slf4j
@RequiredArgsConstructor
public class MammaMindervalideAfspraakOptieAlgoritmeImpl implements MammaAfspraakOptieAlgoritme
{
	private final MammaBaseCapaciteitsBlokService capaciteitsBlokService;

	private static final BigDecimal MINIMALE_CAPACITEIT_NA_MAKEN_MINDERVALIDE_AFSPRAAK = BigDecimal.valueOf(-1);

	@Override
	public MammaAfspraakOptie getAfspraakOptieUitnodiging(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, Integer afspraakBijUitnodigenVanafAantalWerkdagen)
	{
		throw new IllegalStateException("Mindervaliden krijgen geen datumtijd uitnodiging");
	}

	@Override
	public List<MammaAfspraakOptie> getAfspraakOpties(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet, boolean extraOpties,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean corrigeerNegatieveVrijeCapaciteit)
	{
		if (standplaatsPeriode.getStandplaatsRonde().getMinderValideUitwijkStandplaats() != null)
		{
			return Collections.emptyList();
		}

		var nietGeblokkeerdeScreeningCapaciteitBlokDtos = capaciteitsBlokService.getNietGeblokkeerdeScreeningCapaciteitBlokDtos(standplaatsPeriode, toUtilDate(vanaf),
			toUtilDate(totEnMet.atTime(Constants.BK_EINDTIJD_DAG)), dossier.getClient());
		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		var factorMindervalideBk = getFactorMindervalideBk(screeningsEenheid);
		var benodigdeCapaciteitVoorNieuweAfspraakOptie = MammaPlanningUtil.bepaalBenodigdeCapaciteitVoorNieuweAfspraakOptie(factorMindervalideBk, voorlopigeOpkomstkans);
		var vrijeCapaciteitPerDag = bepaalVrijeCapaciteitPerDag(nietGeblokkeerdeScreeningCapaciteitBlokDtos);

		return nietGeblokkeerdeScreeningCapaciteitBlokDtos.stream()
			.filter(blok -> !blok.getMindervalideReserveringen().isEmpty())
			.filter(blok ->
				isGenoegCapaciteitVoorAfspraak(blok, vrijeCapaciteitPerDag, benodigdeCapaciteitVoorNieuweAfspraakOptie))
			.flatMap(blok -> getMindervalideReserveringenBinnenBlok(blok, screeningsEenheid, factorMindervalideBk).stream()
				.map(mvReservering -> maakAfspraakOptie(blok, mvReservering)))
			.toList();
	}

	private boolean isGenoegCapaciteitVoorAfspraak(MammaCapaciteitBlokDto blok, Map<LocalDate, BigDecimal> vrijeCapaciteitPerDag,
		BigDecimal benodigdeCapaciteitVoorNieuweAfspraakOptie)
	{
		var vrijeDagCapaciteit = vrijeCapaciteitPerDag.get(blok.getDatum());
		var vrijeBlokCapaciteit = MammaPlanningUtil.getVrijeCapaciteitVanBlok(blok);

		return heeftVoldoendeCapaciteitOver(vrijeDagCapaciteit, benodigdeCapaciteitVoorNieuweAfspraakOptie)
			&& heeftVoldoendeCapaciteitOver(vrijeBlokCapaciteit, benodigdeCapaciteitVoorNieuweAfspraakOptie);
	}

	private static boolean heeftVoldoendeCapaciteitOver(BigDecimal initieleVrijeCapaciteit, BigDecimal benodigdeCapaciteitVoorNieuweAfspraakOptie)
	{
		return initieleVrijeCapaciteit.subtract(benodigdeCapaciteitVoorNieuweAfspraakOptie).compareTo(MINIMALE_CAPACITEIT_NA_MAKEN_MINDERVALIDE_AFSPRAAK) >= 0;
	}

	private Map<LocalDate, BigDecimal> bepaalVrijeCapaciteitPerDag(Collection<MammaCapaciteitBlokDto> nietGeblokkeerdeScreeningCapaciteitBlokDtos)
	{
		return nietGeblokkeerdeScreeningCapaciteitBlokDtos.stream()
			.collect(Collectors.groupingBy(
				MammaCapaciteitBlokDto::getDatum,
				Collectors.reducing(
					BigDecimal.ZERO,
					MammaPlanningUtil::getVrijeCapaciteitVanBlok,
					BigDecimal::add)
			));
	}

	private BigDecimal getFactorMindervalideBk(MammaScreeningsEenheid screeningsEenheid)
	{
		var screeningOrganisatie = MammaScreeningsEenheidUtil.getScreeningsOrganisatie(screeningsEenheid);
		return screeningOrganisatie.getFactorMinderValideBk();
	}

	private static List<LocalTime> getMindervalideReserveringenBinnenBlok(MammaCapaciteitBlokDto blok, MammaScreeningsEenheid screeningsEenheid, BigDecimal factorMindervalideBk)
	{
		var benodigdeMinutenVoorMindervalideAfspraak = benodigdeMinutenVoorMindervalideAfspraak(factorMindervalideBk);
		var blokRange = Range.closedOpen(blok.getVanaf().toLocalTime(), blok.getTot());

		return blok.getMindervalideReserveringen().stream()
			.filter(reservering ->
			{
				var reserveringRange = Range.closedOpen(reservering, reservering.plusMinutes(benodigdeMinutenVoorMindervalideAfspraak));
				return blokRange.encloses(reserveringRange)
					&& (!isEnkeleMammograaf(screeningsEenheid)
					|| geenOverlapTussenMindervalidereserveringEnBestaandeAfspraak(blok, reserveringRange, factorMindervalideBk));
			})
			.toList();
	}

	private static boolean geenOverlapTussenMindervalidereserveringEnBestaandeAfspraak(MammaCapaciteitBlokDto capaciteitBlokDto,
		Range<LocalTime> reserveringRange, BigDecimal factorMindervalide)
	{
		return capaciteitBlokDto.getAfspraakDtos().stream()
			.noneMatch(afspraakDto -> RangeUtil.isOverlap(reserveringRange,
				Range.closedOpen(afspraakDto.getVanaf().toLocalTime(), getTotTijdAfspraak(afspraakDto, factorMindervalide))));
	}

	private static LocalTime getTotTijdAfspraak(MammaAfspraakDto afspraakDto, BigDecimal factorMindervalide)
	{
		var minutenVoorAfspraak = bepaalBenodigdeMinutenVoorAfspraak(afspraakDto, factorMindervalide);
		return afspraakDto.getVanaf().toLocalTime().plusMinutes(minutenVoorAfspraak);
	}

	private static int bepaalBenodigdeMinutenVoorAfspraak(MammaAfspraakDto afspraakDto, BigDecimal factorMindervalide)
	{
		if (afspraakDto.isMindervalide())
		{
			return MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMindervalide);
		}
		if (afspraakDto.isDubbeleTijd())
		{
			return MammaRationaalAfspraakOptie.BENODIGDE_MINUTEN_VOOR_DUBBELE_TIJD_AFSPRAAK;
		}
		return Constants.BK_TIJDVAK_MIN;
	}

	private MammaAfspraakOptie maakAfspraakOptie(MammaCapaciteitBlokDto blok, LocalTime mvReservering)
	{
		return new MammaBaseAfspraakOptieDto(blok.getId(), blok.getDatum().atTime(mvReservering), blok.getStandplaatsPeriodeId());
	}
}
