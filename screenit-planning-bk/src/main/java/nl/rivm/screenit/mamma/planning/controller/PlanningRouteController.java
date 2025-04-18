package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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
import java.util.NavigableSet;
import java.util.Optional;
import java.util.UUID;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningRouteWijzigenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsPeriodeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsRondeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_ROUTE)
public class PlanningRouteController
{
	private final ICurrentDateSupplier dateSupplier;

	private final MammaBaseAfspraakService baseAfspraakService;

	public PlanningRouteController(ICurrentDateSupplier dateSupplier, MammaBaseAfspraakService baseAfspraakService)
	{
		this.dateSupplier = dateSupplier;
		this.baseAfspraakService = baseAfspraakService;
	}

	public static PlanningStandplaatsPeriode decrementIndex(int index, int movedItemVolgNr, PlanningScreeningsEenheid screeningsEenheid)
	{
		List<PlanningStandplaatsPeriode> updatedPeriodes = new ArrayList<>();
		NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
		for (PlanningStandplaatsPeriode periode : standplaatsPeriodeNavigableSet)
		{
			if (periode.getScreeningsEenheidVolgNr() <= index && periode.getScreeningsEenheidVolgNr() > movedItemVolgNr)
			{
				periode.setScreeningsEenheidVolgNr(periode.getScreeningsEenheidVolgNr() - 1);
				updatedPeriodes.add(periode);
			}
		}
		return getHighestPeriode(updatedPeriodes, null);
	}

	private static PlanningStandplaatsPeriode incrementIndex(int index, int movedItemVolgNr, PlanningScreeningsEenheid screeningsEenheid)
	{
		List<PlanningStandplaatsPeriode> updatedPeriodes = new ArrayList<>();
		for (PlanningStandplaatsPeriode periode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
		{
			if (periode.getScreeningsEenheidVolgNr() < movedItemVolgNr && periode.getScreeningsEenheidVolgNr() >= index)
			{
				periode.setScreeningsEenheidVolgNr(periode.getScreeningsEenheidVolgNr() + 1);
				updatedPeriodes.add(periode);
			}
		}
		return getHighestPeriode(updatedPeriodes, null);
	}

	private static PlanningStandplaatsPeriode getHighestPeriode(List<PlanningStandplaatsPeriode> updatedPeriodes, PlanningStandplaatsPeriode currentHighestPeriode)
	{
		PlanningStandplaatsPeriode highestPeriode = currentHighestPeriode;
		for (PlanningStandplaatsPeriode periode : updatedPeriodes)
		{
			if (highestPeriode == null || highestPeriode.getScreeningsEenheidVolgNr() > periode.getScreeningsEenheidVolgNr())
			{
				highestPeriode = periode;
			}
		}
		return highestPeriode;
	}

	@GetMapping(value = "/{screeningsEenheidId}")
	public List<PlanningStandplaatsPeriodeDto> get(@PathVariable Long screeningsEenheidId)
	{
		List<PlanningStandplaatsPeriodeDto> standplaatsPeriodeDtoList = new ArrayList<>();
		for (PlanningStandplaatsPeriode standplaatsPeriode : PlanningScreeningsEenheidIndex.get(screeningsEenheidId).getStandplaatsPeriodeNavigableSet())
		{
			standplaatsPeriodeDtoList.add(PlanningMapper.from(standplaatsPeriode));
		}
		return standplaatsPeriodeDtoList;
	}

	@PutMapping
	public void put(@RequestBody PlanningRouteWijzigenDto gewijzigdeStandplaatsPeriodeDto)
	{
		PlanningScreeningsEenheid screeningsEenheidNaar = PlanningScreeningsEenheidIndex.get(gewijzigdeStandplaatsPeriodeDto.screeningsEenheidId);

		if (!gewijzigdeStandplaatsPeriodeMagConceptOverschrijven(gewijzigdeStandplaatsPeriodeDto))
		{
			return;
		}

		PlanningStandplaatsPeriode standplaatsPeriode;
		PlanningStandplaatsPeriode gewijzigdVanafPeriode = null;
		if (gewijzigdeStandplaatsPeriodeDto.standplaatsPeriodeConceptId == null)
		{
			gewijzigdeStandplaatsPeriodeDto.volgNr = getNieuwIniVolgNr(screeningsEenheidNaar);
			standplaatsPeriode = new PlanningStandplaatsPeriode(null, gewijzigdeStandplaatsPeriodeDto.volgNr, 1, dateSupplier.getLocalDate(), true,
				PlanningConstanten.plannenTotEnMetDatum); 
			screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);

			PlanningStandplaatsRonde standplaatsRonde = new PlanningStandplaatsRonde(null, null, null, null, null, false, null, BigDecimal.ZERO);
			PlanningStandplaats standplaats = PlanningStandplaatsIndex.get(gewijzigdeStandplaatsPeriodeDto.standplaatsId);
			standplaatsRonde.setStandplaats(standplaats);
			standplaatsRonde.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);
			standplaats.getStandplaatsRondeNavigableSet().add(standplaatsRonde);

			standplaatsPeriode.setStandplaatsRonde(standplaatsRonde);
			standplaatsPeriode.setScreeningsEenheid(screeningsEenheidNaar);

			PlanningStandplaatsPeriodeIndex.put(standplaatsPeriode);
			PlanningStandplaatsRondeIndex.put(standplaatsRonde);
			gewijzigdVanafPeriode = standplaatsPeriode;
		}
		else
		{
			standplaatsPeriode = PlanningStandplaatsPeriodeIndex.get(gewijzigdeStandplaatsPeriodeDto.standplaatsPeriodeConceptId);
		}

		PlanningScreeningsEenheid screeningsEenheidVan = standplaatsPeriode.getScreeningsEenheid();
		boolean blijftBinnenDeScreeningsEenheid = screeningsEenheidNaar.equals(screeningsEenheidVan);
		NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodenVan = screeningsEenheidVan.getStandplaatsPeriodeNavigableSet();
		if (!blijftBinnenDeScreeningsEenheid)
		{
			PlanningWijzigingen.getWijzigingenRoute(screeningsEenheidVan).setVanafStandplaatsPeriode(standplaatsPeriodenVan.higher(standplaatsPeriode));
			standplaatsPeriodenVan.remove(standplaatsPeriode);
			standplaatsPeriode.setScreeningsEenheid(screeningsEenheidNaar);
		}
		int nieuweVolgNr = gewijzigdeStandplaatsPeriodeDto.volgNr;
		int oudeVolgNr = standplaatsPeriode.getScreeningsEenheidVolgNr();
		if (nieuweVolgNr != oudeVolgNr || !blijftBinnenDeScreeningsEenheid)
		{
			if (blijftBinnenDeScreeningsEenheid)
			{
				screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().remove(standplaatsPeriode);

				if (nieuweVolgNr > oudeVolgNr)
				{
					gewijzigdVanafPeriode = decrementIndex(nieuweVolgNr, oudeVolgNr, screeningsEenheidNaar);
				}
				if (nieuweVolgNr < oudeVolgNr)
				{
					gewijzigdVanafPeriode = incrementIndex(nieuweVolgNr, oudeVolgNr, screeningsEenheidNaar);
				}

				standplaatsPeriode.setScreeningsEenheidVolgNr(nieuweVolgNr);
				screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);
			}
			else
			{
				int index = getNieuwIniVolgNr(screeningsEenheidVan);
				decrementIndex(index, oudeVolgNr, screeningsEenheidVan);
				int totEnMetVolgnummer = screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().isEmpty() ? 0
					: screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().last().getScreeningsEenheidVolgNr() + 1;

				gewijzigdVanafPeriode = incrementIndex(nieuweVolgNr, totEnMetVolgnummer, screeningsEenheidNaar);
				standplaatsPeriode.setScreeningsEenheidVolgNr(nieuweVolgNr);
				screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);
				standplaatsPeriode.setScreeningsEenheid(screeningsEenheidNaar);
			}
			gewijzigdVanafPeriode = getHighestPeriode(List.of(standplaatsPeriode), gewijzigdVanafPeriode);
		}
		PlanningWijzigingen.getWijzigingenRoute(screeningsEenheidNaar).setVanafStandplaatsPeriode(gewijzigdVanafPeriode);

		PlanningDoorrekenenManager.run();
	}

	private boolean gewijzigdeStandplaatsPeriodeMagConceptOverschrijven(PlanningRouteWijzigenDto gewijzigdeStandplaatsPeriodeDto)
	{
		var screeningsEenheidNaar = PlanningScreeningsEenheidIndex.get(gewijzigdeStandplaatsPeriodeDto.screeningsEenheidId);

		return screeningsEenheidNaar.getStandplaatsPeriodeNavigableSet().stream()
			.noneMatch(p ->
			{
				var teControlerenVolgnummer = p.getScreeningsEenheidVolgNr();
				return teControlerenVolgnummer != null && teControlerenVolgnummer.equals(gewijzigdeStandplaatsPeriodeDto.volgNr) && Boolean.FALSE.equals(p.getPrognose());
			});
	}

	@PutMapping(value = "/splitsStandplaatsPeriode/{standplaatsPeriodeConceptId}")
	public void splitsStandplaatsPeriode(@PathVariable UUID standplaatsPeriodeConceptId)
	{
		PlanningStandplaatsPeriode standplaatsPeriode = PlanningStandplaatsPeriodeIndex.get(standplaatsPeriodeConceptId);
		PlanningScreeningsEenheid screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();

		Optional<PlanningStandplaatsPeriode> eersteStandplaatsPeriodeMetPrognose = getEersteStandplaatsPeriodeMetPrognoseGeenAfspraken(screeningsEenheid);
		int volgNr = eersteStandplaatsPeriodeMetPrognose.isPresent() ? eersteStandplaatsPeriodeMetPrognose.get().getScreeningsEenheidVolgNr()
			: screeningsEenheid.getStandplaatsPeriodeNavigableSet().last().getScreeningsEenheidVolgNr() + 1;

		incrementIndex(volgNr, screeningsEenheid.getStandplaatsPeriodeNavigableSet().last().getScreeningsEenheidVolgNr() + 1,
			screeningsEenheid);

		PlanningStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();

		int volgNrInStandplaatsRonde = standplaatsPeriode.getStandplaatsRondeVolgNr() + 1;

		PlanningStandplaatsPeriode nieuweStandplaatsPeriode = new PlanningStandplaatsPeriode(null, volgNr, volgNrInStandplaatsRonde, dateSupplier.getLocalDate(), true,
			PlanningConstanten.plannenTotEnMetDatum); 

		standplaatsRonde.getStandplaatsPeriodeNavigableSet().add(nieuweStandplaatsPeriode);
		nieuweStandplaatsPeriode.setStandplaatsRonde(standplaatsRonde);

		screeningsEenheid.getStandplaatsPeriodeNavigableSet().add(nieuweStandplaatsPeriode);
		nieuweStandplaatsPeriode.setScreeningsEenheid(screeningsEenheid);

		PlanningStandplaatsPeriodeIndex.put(nieuweStandplaatsPeriode);
		PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).setVanafStandplaatsPeriode(nieuweStandplaatsPeriode);

		PlanningDoorrekenenManager.run();
	}

	private int getNieuwIniVolgNr(PlanningScreeningsEenheid screeningsEenheid)
	{
		int nieuwVolgNr = 0;
		NavigableSet<PlanningStandplaatsPeriode> perioden = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
		if (!perioden.isEmpty())
		{
			nieuwVolgNr = perioden.last().getScreeningsEenheidVolgNr() + 1;
		}
		else
		{
			nieuwVolgNr = screeningsEenheid.getVolgNrOffset();
		}
		return nieuwVolgNr;
	}

	private Optional<PlanningStandplaatsPeriode> getEersteStandplaatsPeriodeMetPrognoseGeenAfspraken(PlanningScreeningsEenheid screeningsEenheid)
	{
		NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
		return standplaatsPeriodeNavigableSet.stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getPrognose()
				&& (standplaatsPeriode.getId() == null || !baseAfspraakService.heeftAfspraken(standplaatsPeriode.getId(), MammaAfspraakStatus.GEPLAND)))
			.findFirst();
	}

}
