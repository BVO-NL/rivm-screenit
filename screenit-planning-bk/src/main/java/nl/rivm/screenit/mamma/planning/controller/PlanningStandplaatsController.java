package nl.rivm.screenit.mamma.planning.controller;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningAfspraakDrempelOverzichtDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.service.PlanningAfspraakDrempelOverzichtService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_STANDPLAATS)
public class PlanningStandplaatsController
{

	private final PlanningAfspraakDrempelOverzichtService afspraakDrempelOverzichtService;

	public PlanningStandplaatsController(PlanningAfspraakDrempelOverzichtService afspraakDrempelOverzichtService)
	{
		this.afspraakDrempelOverzichtService = afspraakDrempelOverzichtService;
	}

	@PostMapping
	public void post(@RequestBody PlanningStandplaatsDto standplaatsDto)
	{
		addOrChangeStandplaats(standplaatsDto);
	}

	@PutMapping
	public void put(@RequestBody PlanningStandplaatsDto standplaatsDto)
	{
		addOrChangeStandplaats(standplaatsDto);
	}

	@GetMapping("/zonderRoute/{screeningsOrganisatieId}")
	public ResponseEntity<Long[]> getZonderRoute(@PathVariable Long screeningsOrganisatieId)
	{
		var standplaatsenZonderRonde = PlanningStandplaatsIndex.getStandplaatsenZonderRoute(screeningsOrganisatieId);
		var response = new ResponseEntity<Long[]>(standplaatsenZonderRonde.toArray(new Long[] {}), HttpStatus.OK);
		return response;
	}

	@GetMapping("/metRoute/{screeningsOrganisatieId}")
	public ResponseEntity<Long[]> getMetRoute(@PathVariable Long screeningsOrganisatieId)
	{
		var standplaatsenMetRonde = PlanningStandplaatsIndex.getStandplaatsenMetRoute(screeningsOrganisatieId);
		var response = new ResponseEntity<Long[]>(standplaatsenMetRonde.toArray(new Long[] {}), HttpStatus.OK);
		return response;
	}

	@DeleteMapping("/{standplaatsId}")
	public void delete(@PathVariable Long standplaatsId)
	{
		var knownStandplaats = PlanningStandplaatsIndex.get(standplaatsId);
		if (knownStandplaats != null)
		{
			knownStandplaats.getScreeningsOrganisatie().getStandplaatsSet().remove(knownStandplaats);

			for (var standplaatsRonde : knownStandplaats.getStandplaatsRondeNavigableSet())
			{
				for (var standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
				{
					var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
					var standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
					var volgendeStandplaatsPeriode = standplaatsPeriodeNavigableSet.higher(standplaatsPeriode);
					standplaatsPeriodeNavigableSet.remove(standplaatsPeriode);
					if (volgendeStandplaatsPeriode != null)
					{
						PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).setVanafStandplaatsPeriode(
							PlanningRouteController.decrementIndex(standplaatsPeriodeNavigableSet.size() + 1, standplaatsPeriode.getScreeningsEenheidVolgNr(), screeningsEenheid));
					}
					else
					{
						PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid)
							.setVanafStandplaatsPeriode(!standplaatsPeriodeNavigableSet.isEmpty() ? standplaatsPeriodeNavigableSet.last() : null);
					}
				}
			}
			PlanningStandplaatsIndex.remove(knownStandplaats);
			PlanningDoorrekenenManager.run();
		}
	}

	private void addOrChangeStandplaats(PlanningStandplaatsDto standplaatsDto)
	{
		var screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(standplaatsDto.screeningsOrganisatieId);
		var knownStandplaats = PlanningStandplaatsIndex.get(standplaatsDto.id);
		if (knownStandplaats == null)
		{
			knownStandplaats = new PlanningStandplaats(standplaatsDto.id);
			knownStandplaats.setScreeningsOrganisatie(screeningsOrganisatie);
			PlanningStandplaatsIndex.put(knownStandplaats);
		}
		if (!knownStandplaats.getScreeningsOrganisatie().equals(screeningsOrganisatie))
		{
			knownStandplaats.getScreeningsOrganisatie().getStandplaatsSet().remove(knownStandplaats);
			knownStandplaats.setScreeningsOrganisatie(screeningsOrganisatie);
		}
		screeningsOrganisatie.getStandplaatsSet().add(knownStandplaats);

		PlanningWijzigingen.getStandplaatsSet().add(knownStandplaats);
	}

	@GetMapping("/getAfspraakDrempelOverzicht/{standplaatsId}")
	public ResponseEntity<PlanningAfspraakDrempelOverzichtDto> getAfspraakDrempelOverzicht(@PathVariable long standplaatsId)
	{
		var standplaats = PlanningStandplaatsIndex.get(standplaatsId);
		return new ResponseEntity<>(afspraakDrempelOverzichtService.getAfspraakDrempelOverzicht(standplaats), HttpStatus.OK);
	}
}
