package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.time.LocalTime;
import java.util.Comparator;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMindervalideReserveringDto;
import nl.rivm.screenit.main.exception.MammaMinderValideReserveringException;
import nl.rivm.screenit.main.service.mamma.MammaMinderValideReserveringService;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.util.RangeUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

import org.hibernate.Hibernate;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

import static nl.rivm.screenit.util.DateUtil.toLocalDateTime;
import static nl.rivm.screenit.util.DateUtil.toLocalTime;

@Service
@RequiredArgsConstructor
public class MammaMinderValideReserveringServiceImpl implements MammaMinderValideReserveringService
{
	private final MammaScreeningsEenheidRepository screeningsEenheidRepository;

	@Override
	public PlanningMindervalideReserveringDto maakEerstBeschikbareMinderValideReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
		throws MammaMinderValideReserveringException
	{
		return new PlanningMindervalideReserveringDto(null, null, getEerstBeschikbareMomentVoorMinderValideReservering(planningCapaciteitBlokDto));
	}

	@Override
	public int getBenodigdeMinutenVoorReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
	{
		var factorMinderValide = getFactorMinderValideBijBlok(planningCapaciteitBlokDto);
		return MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMinderValide);
	}

	@Override
	public void valideerMinderValideReserveringen(PlanningCapaciteitBlokDto capaciteitBlokDto)
		throws MammaMinderValideReserveringException
	{
		var mindervalideReserveringDtos = capaciteitBlokDto.getMinderValideReserveringen();

		var factorMinderValide = getFactorMinderValideBijBlok(capaciteitBlokDto);
		valideerVoldoendeCapaciteitVoorReserveringen(capaciteitBlokDto, mindervalideReserveringDtos, factorMinderValide, false);

		var benodigdeMinutenVoorReservering = MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMinderValide);
		var reserveringRanges = maakLijstMetReserveringRangesVoorReserveringen(mindervalideReserveringDtos, benodigdeMinutenVoorReservering);

		var isErOverlap = reserveringRanges.stream()
			.anyMatch(range1 -> reserveringRanges.stream()
				.anyMatch(range2 -> range1 != range2 && RangeUtil.isOverlap(range1, range2)));

		if (isErOverlap)
		{
			throw new MammaMinderValideReserveringException("overlap.tussen.mindervalide.reserveringen");
		}

		var capaciteitBlokRange = capaciteitBlokDto.getCapaciteitBlokRange();
		if (reserveringRanges.stream().anyMatch(range -> !capaciteitBlokRange.encloses(range)))
		{
			throw new MammaMinderValideReserveringException("mindervalide.reservering.buiten.capaciteit.blok");
		}
	}

	private LocalTime getEerstBeschikbareMomentVoorMinderValideReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
		throws MammaMinderValideReserveringException
	{
		var mindervalideReserveringDtos = planningCapaciteitBlokDto.getMinderValideReserveringen();
		var factorMinderValide = getFactorMinderValideBijBlok(planningCapaciteitBlokDto);
		var benodigdeAantalMinutenVoorReservering = MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak(factorMinderValide);
		valideerVoldoendeCapaciteitVoorReserveringen(planningCapaciteitBlokDto, mindervalideReserveringDtos, factorMinderValide, true);
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
				throw new MammaMinderValideReserveringException("geen.plaats.voor.mindervalide.reservering");
			}
		}
		return berekenEerstVolgendeVrijeMomentVoorReservering(planningCapaciteitBlokDto, mindervalideReserveringDtos, benodigdeAantalMinutenVoorReservering);
	}

	private static void valideerVoldoendeCapaciteitVoorReserveringen(PlanningCapaciteitBlokDto capaciteitBlokDto,
		List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos, BigDecimal factorMinderValide, boolean isVoorNieuweReservering)
		throws MammaMinderValideReserveringException
	{
		var correctieVoorAanmakenNieuweMinderValideReservering = isVoorNieuweReservering ? 1 : 0;
		var capaciteitBenodigd = BigDecimal.valueOf(mindervalideReserveringDtos.size() + correctieVoorAanmakenNieuweMinderValideReservering).multiply(factorMinderValide);
		if (capaciteitBenodigd.compareTo(BigDecimal.valueOf(capaciteitBlokDto.aantalOnderzoeken)) > 0)
		{
			throw new MammaMinderValideReserveringException("te.veel.mindervalide.reserveringen.voor.capaciteit");
		}
	}

	private LocalTime berekenEerstVolgendeVrijeMomentVoorReservering(PlanningCapaciteitBlokDto planningCapaciteitBlokDto,
		List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos, int benodigdeMinutenVoorReservering)
		throws MammaMinderValideReserveringException
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
		throw new MammaMinderValideReserveringException("geen.plaats.voor.mindervalide.reservering");
	}

	private List<Range<LocalTime>> maakLijstMetReserveringRangesVoorReserveringen(List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos,
		int benodigdeMinutenVoorReservering)
	{
		return mindervalideReserveringDtos.stream().sorted(Comparator.comparing(PlanningMindervalideReserveringDto::getVanaf)).map(dto ->
			Range.closedOpen(dto.getVanaf(), dto.getVanaf().plusMinutes(benodigdeMinutenVoorReservering))
		).toList();
	}

	private BigDecimal getFactorMinderValideBijBlok(PlanningCapaciteitBlokDto planningCapaciteitBlokDto)
	{
		var screeningsEenheid = screeningsEenheidRepository.findById(planningCapaciteitBlokDto.screeningsEenheidId).orElseThrow();
		var screeningOrganisatie = (ScreeningOrganisatie) Hibernate.unproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		return screeningOrganisatie.getFactorMinderValideBk();
	}
}
