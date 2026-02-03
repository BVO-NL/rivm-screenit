package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.mamma.afspraken.MammaMindervalideReserveringProjectie;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMindervalideReserveringDto;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.controller.PlanningCapaciteitChangeChecker;
import nl.rivm.screenit.mamma.planning.controller.PlanningMapper;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningMindervalideReservering;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.repository.PlanningCapaciteitBlokRepository;
import nl.rivm.screenit.mamma.planning.repository.PlanningMindervalideReserveringRepository;
import nl.rivm.screenit.mamma.planning.repository.projectie.PlanningCapaciteitBlokProjectie;
import nl.rivm.screenit.mamma.planning.service.PlanningCapaciteitBlokService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RangeUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.BoundType;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class PlanningCapaciteitBlokServiceImpl implements PlanningCapaciteitBlokService
{
	private final PlanningCapaciteitBlokRepository capaciteitBlokRepository;

	private final PlanningMindervalideReserveringRepository mindervalideReserveringRepository;

	@Override
	public Set<PlanningBlok> getCapaciteitsBlokkenVanDag(PlanningScreeningsEenheid screeningsEenheid, LocalDate bronDate)
	{
		if (bronDate.isBefore(PlanningConstanten.prognoseVanafDatum))
		{
			return leesCapaciteitBlokken(screeningsEenheid, bronDate, bronDate);
		}
		else
		{
			var dag = screeningsEenheid.getDag(bronDate);
			return dag == null ? Collections.emptySet() : dag.getBlokSet();
		}
	}

	@Override
	public Set<PlanningBlok> leesCapaciteitBlokken(PlanningScreeningsEenheid screeningsEenheid, LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		var zoekPeriode = RangeUtil.range(vanafDatum, BoundType.CLOSED, totEnMetDatum, BoundType.CLOSED);

		var reserveringPerCapaciteitBlokId = mindervalideReserveringRepository.leesMindervalideReserveringen(screeningsEenheid, zoekPeriode).stream().collect(
			Collectors.groupingBy(MammaMindervalideReserveringProjectie::capaciteitBlokId,
				Collectors.mapping(projectie -> new PlanningMindervalideReservering(projectie.id(), projectie.vanaf()), Collectors.toList())));

		var planningBlokken = capaciteitBlokRepository.leesCapaciteitBlokken(screeningsEenheid, zoekPeriode);

		return planningBlokken.stream().map(
				blokProjectie -> mapNaarPlanningBlok(screeningsEenheid, blokProjectie, reserveringPerCapaciteitBlokId.getOrDefault(blokProjectie.getId(), Collections.emptyList())))
			.collect(Collectors.toSet());
	}

	private PlanningBlok mapNaarPlanningBlok(PlanningScreeningsEenheid screeningsEenheid, PlanningCapaciteitBlokProjectie blokProjectie,
		List<PlanningMindervalideReservering> mindervalideReserveringen)
	{
		var vanaf = blokProjectie.getVanaf();
		var blok = new PlanningBlok(blokProjectie.getId(), vanaf.toLocalTime(), blokProjectie.getTot().toLocalTime(), blokProjectie.getAantalOnderzoeken(),
			blokProjectie.getBlokType(), blokProjectie.getOpmerkingen(), blokProjectie.isMinderValideAfspraakMogelijk(), mindervalideReserveringen);
		var dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
		blok.setDag(dag);
		blok.setScreeningsEenheid(screeningsEenheid);
		return blok;
	}

	@Override
	public PlanningBlok maakBlok(PlanningCapaciteitBlokDto blokDto) throws OpslaanVerwijderenTijdBlokException
	{
		var screeningsEenheid = PlanningScreeningsEenheidIndex.get(blokDto.screeningsEenheidId);

		PlanningCapaciteitChangeChecker.magCapaciteitOpslaanVerwijderen(blokDto, true);

		var vanaf = DateUtil.toLocalDateTime(blokDto.vanaf);
		var mindervalideReserveringen = maakMindervalideReserveringen(blokDto);
		var blok = new PlanningBlok(null, vanaf.toLocalTime(), DateUtil.toLocalTime(blokDto.tot), blokDto.aantalOnderzoeken, blokDto.blokType, blokDto.opmerkingen,
			blokDto.minderValideAfspraakMogelijk, mindervalideReserveringen);
		blok.setScreeningsEenheid(screeningsEenheid);
		screeningsEenheid.getBlokSet().add(blok);

		var dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
		dag.getBlokSet().add(blok);
		blok.setDag(dag);
		PlanningBlokIndex.put(blok);

		PlanningBlokIndex.changed(screeningsEenheid, Collections.singleton(blok));
		PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).getBlokSet().add(blok);
		return blok;
	}

	private List<PlanningMindervalideReservering> maakMindervalideReserveringen(PlanningCapaciteitBlokDto blokDto)
	{
		return blokDto.getMindervalideReserveringen().stream().map(dto -> new PlanningMindervalideReservering(null, dto.getVanaf())).toList();
	}

	@Override
	public void verwijderBlok(PlanningBlok blok) throws OpslaanVerwijderenTijdBlokException
	{
		PlanningCapaciteitChangeChecker.magCapaciteitOpslaanVerwijderen(PlanningMapper.from(blok), false);
		PlanningBlokIndex.deleted(blok.getScreeningsEenheid(), Collections.singleton(blok));

		var dag = blok.getDag();
		dag.getBlokSet().remove(blok);
	}

	@Override
	public void updateMindervalideReserveringenVoorCapaciteitBlok(List<PlanningMindervalideReserveringDto> mindervalideReserveringDtos, PlanningBlok capaciteitBlok)
	{
		var nieuweReserveringen = mindervalideReserveringDtos.stream().map(dto ->
		{
			var reserveringVanaf = dto.getVanaf();
			return dto.conceptId == null ?
				new PlanningMindervalideReservering(dto.getId(), reserveringVanaf) :
				new PlanningMindervalideReservering(dto.getId(), reserveringVanaf, dto.conceptId);
		}).toList();
		var planningMindervalideReserveringen = capaciteitBlok.getMindervalideReserveringen();
		planningMindervalideReserveringen.clear();
		planningMindervalideReserveringen.addAll(nieuweReserveringen);
	}
}
