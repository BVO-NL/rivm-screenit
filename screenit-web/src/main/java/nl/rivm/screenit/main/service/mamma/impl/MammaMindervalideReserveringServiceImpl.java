package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Comparator;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMindervalideReserveringDto;
import nl.rivm.screenit.main.exception.MammaMindervalideReserveringException;
import nl.rivm.screenit.main.service.mamma.MammaMindervalideReserveringService;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.util.RangeUtil;
import nl.rivm.screenit.util.mamma.MammaMindervalideUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil;

import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

import static nl.rivm.screenit.util.DateUtil.toLocalDateTime;
import static nl.rivm.screenit.util.DateUtil.toLocalTime;

@Service
@RequiredArgsConstructor
public class MammaMindervalideReserveringServiceImpl implements MammaMindervalideReserveringService
{
	private final MammaScreeningsEenheidRepository screeningsEenheidRepository;

	@Override
	public PlanningMindervalideReserveringDto maakEerstBeschikbareMindervalideReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
		throws MammaMindervalideReserveringException
	{
		return new PlanningMindervalideReserveringDto(null, null, getEerstBeschikbareMomentVoorMindervalideReservering(planningCapaciteitBlokDto));
	}

	@Override
	public int getBenodigdeMinutenVoorReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
	{
		var factorMindervalide = getFactorMindervalideBijBlok(planningCapaciteitBlokDto);
		return MammaMindervalideUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMindervalide);
	}

	@Override
	public void valideerMindervalideReserveringen(PlanningCapaciteitBlokDto capaciteitBlokDto)
		throws MammaMindervalideReserveringException
	{
		var mindervalideReserveringDtos = capaciteitBlokDto.getMindervalideReserveringen();

		var factorMindervalide = getFactorMindervalideBijBlok(capaciteitBlokDto);
		valideerVoldoendeCapaciteitVoorReserveringen(capaciteitBlokDto, mindervalideReserveringDtos, factorMindervalide, false);

		var benodigdeMinutenVoorReservering = MammaMindervalideUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMindervalide);
		var reserveringRanges = maakLijstMetReserveringRangesVoorReserveringen(mindervalideReserveringDtos, benodigdeMinutenVoorReservering);

		var isErOverlap = reserveringRanges.stream()
			.anyMatch(range1 -> reserveringRanges.stream()
				.anyMatch(range2 -> range1 != range2 && RangeUtil.isOverlap(range1, range2)));

		if (isErOverlap)
		{
			throw new MammaMindervalideReserveringException("overlap.tussen.mindervalide.reserveringen");
		}

		var capaciteitBlokRange = capaciteitBlokDto.getCapaciteitBlokRange();
		if (reserveringRanges.stream().anyMatch(range -> !capaciteitBlokRange.encloses(range)))
		{
			throw new MammaMindervalideReserveringException("mindervalide.reservering.buiten.capaciteit.blok");
		}
	}

	private LocalTime getEerstBeschikbareMomentVoorMindervalideReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
		throws MammaMindervalideReserveringException
	{
		var mindervalideReserveringDtos = planningCapaciteitBlokDto.getMindervalideReserveringen();
		var factorMindervalide = getFactorMindervalideBijBlok(planningCapaciteitBlokDto);
		var benodigdeAantalMinutenVoorReservering = MammaMindervalideUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMindervalide);
		valideerVoldoendeCapaciteitVoorReserveringen(planningCapaciteitBlokDto, mindervalideReserveringDtos, factorMindervalide, true);
		if (mindervalideReserveringDtos.isEmpty())
		{
			var vanaf = toLocalDateTime(planningCapaciteitBlokDto.vanaf);
			var tot = toLocalDateTime(planningCapaciteitBlokDto.tot);
			if (!vanaf.plusMinutes(benodigdeAantalMinutenVoorReservering).isAfter(tot))
			{
				return vanaf.toLocalTime();
			}
			else
			{
				throw new MammaMindervalideReserveringException("geen.plaats.voor.mindervalide.reservering");
			}
		}
		return berekenEerstVolgendeVrijeMomentVoorReservering(planningCapaciteitBlokDto, mindervalideReserveringDtos, benodigdeAantalMinutenVoorReservering);
	}

	private static void valideerVoldoendeCapaciteitVoorReserveringen(PlanningCapaciteitBlokDto capaciteitBlokDto,
		List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos, BigDecimal factorMindervalide, boolean isVoorNieuweReservering)
		throws MammaMindervalideReserveringException
	{
		var correctieVoorAanmakenNieuweMindervalideReservering = isVoorNieuweReservering ? 1 : 0;
		var capaciteitBenodigd = BigDecimal.valueOf(mindervalideReserveringDtos.size() + correctieVoorAanmakenNieuweMindervalideReservering).multiply(factorMindervalide);
		if (capaciteitBenodigd.compareTo(BigDecimal.valueOf(capaciteitBlokDto.aantalOnderzoeken)) > 0)
		{
			throw new MammaMindervalideReserveringException(
				isVoorNieuweReservering ? "onvoldoende.capaciteit.voor.nieuwe.mindervalidenreserveringen" : "onvoldoende.capaciteit.voor.mindervalidenreserveringen");
		}
	}

	private LocalTime berekenEerstVolgendeVrijeMomentVoorReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto,
		List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos, int benodigdeMinutenVoorReservering)
		throws MammaMindervalideReserveringException
	{
		var bezetteRanges = maakLijstMetReserveringRangesVoorReserveringen(mindervalideReserveringDtos, benodigdeMinutenVoorReservering);

		var zoekVoorRuimteVanaf = toLocalTime(planningCapaciteitBlokDto.vanaf);
		for (var bezetteRange : bezetteRanges)
		{
			var potentieelEersteMoment = Range.closedOpen(zoekVoorRuimteVanaf, zoekVoorRuimteVanaf.plusMinutes(benodigdeMinutenVoorReservering));
			if (RangeUtil.isOverlap(bezetteRange, potentieelEersteMoment))
			{
				zoekVoorRuimteVanaf = bezetteRange.upperEndpoint();
			}
			else
			{
				return zoekVoorRuimteVanaf;
			}
		}
		if (!zoekVoorRuimteVanaf.plusMinutes(benodigdeMinutenVoorReservering).isAfter(toLocalTime(planningCapaciteitBlokDto.tot)))
		{
			return zoekVoorRuimteVanaf;
		}
		throw new MammaMindervalideReserveringException("geen.plaats.voor.mindervalide.reservering");
	}

	private List<Range<LocalTime>> maakLijstMetReserveringRangesVoorReserveringen(List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos,
		int benodigdeMinutenVoorReservering)
	{
		return mindervalideReserveringDtos.stream().sorted(Comparator.comparing(PlanningMindervalideReserveringDto::getVanaf)).map(dto ->
			Range.closedOpen(dto.getVanaf(), dto.getVanaf().plusMinutes(benodigdeMinutenVoorReservering))
		).toList();
	}

	private BigDecimal getFactorMindervalideBijBlok(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
	{
		var screeningsEenheid = screeningsEenheidRepository.findById(planningCapaciteitBlokDto.screeningsEenheidId).orElseThrow();
		var screeningOrganisatie = MammaScreeningsEenheidUtil.getScreeningsOrganisatie(screeningsEenheid);
		return screeningOrganisatie.getFactorMindervalideBk();
	}
}
