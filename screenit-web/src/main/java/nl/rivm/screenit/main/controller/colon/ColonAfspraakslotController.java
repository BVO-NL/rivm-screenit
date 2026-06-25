package nl.rivm.screenit.main.controller.colon;

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

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/colon/rooster/afspraakslot")
@Tag(name = "Colon afspraakslots", description = "Beheer van afspraakslots voor het colon-rooster")
public class ColonAfspraakslotController
{
	private final ColonAfspraakslotService afspraakslotService;

	@GetMapping
	@Operation(summary = "Haal afspraakslots op", description = "Geeft afspraakslots terug voor de opgegeven periode van de ingelogde intakelocatie.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakslots succesvol opgehaald"),
		@ApiResponse(responseCode = "400", description = "Ongeldig verzoek"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonAfspraakslotDto> getAfspraakslots(
		@RequestParam("startDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
		@RequestParam("endDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate)
	{
		var intakeLocatie = ScreenitSession.get().getIntakelocatie();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("Medewerker heeft geen intakelocatie.");
		}

		if (startDate == null)
		{
			throw new IllegalStateException("Er is geen start datum meegegeven");
		}

		if (endDate == null)
		{
			throw new IllegalStateException("Er is geen eind datum meegegeven");
		}

		return afspraakslotService.getAfspraakslots(startDate, endDate, intakeLocatie);
	}

	@GetMapping("/search")
	@Operation(summary = "Zoek afspraakslots", description = "Zoekt afspraakslots op basis van datum, tijd, kamer en weekdagen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakslots succesvol gevonden"),
		@ApiResponse(responseCode = "400", description = "Ongeldig verzoek"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonTijdslotDto> searchAfspraakslots(
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDatum,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate eindDatum,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.TIME) LocalTime startTijd,
		@RequestParam() @DateTimeFormat(iso = DateTimeFormat.ISO.TIME) LocalTime eindTijd,
		@Parameter(description = "Optioneel kamer-id") @RequestParam(required = false) Long kamerId,
		@Parameter(description = "Weekdagen als komma-gescheiden lijst van nummers") @RequestParam() String dagen)
	{
		var intakelocatie = ScreenitSession.get().getIntakelocatie();

		if (intakelocatie == null)
		{
			throw new IllegalStateException("Medewerker heeft geen intakelocatie.");
		}

		var filter = new RoosterListViewFilter();
		filter.setStartDatum(DateUtil.toUtilDate(startDatum));
		filter.setEindDatum(DateUtil.toUtilDate(eindDatum));
		filter.setStartTijd(startTijd);
		filter.setEindTijd(eindTijd);
		filter.setKamerId(kamerId);
		filter.setDagen(Stream.of(dagen.split(","))
			.map(String::trim)
			.map(Integer::parseInt)
			.collect(Collectors.toList()));

		return afspraakslotService.searchAfspraakslots(filter, intakelocatie.getId());
	}

	@PostMapping
	@Operation(summary = "Maak afspraakslots aan", description = "Maakt één of meer afspraakslots aan voor de ingelogde intakelocatie.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "201", description = "Afspraakslots succesvol aangemaakt")
	})
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> createAfspraakslots(@Parameter(description = "Gegevens van de aan te maken afspraakslots") @RequestBody ColonAfspraakslotDto afspraakslotsDto)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException, BulkAanmakenException
	{
		afspraakslotService.createAfspraakslot(afspraakslotsDto, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.status(HttpStatus.CREATED).build();
	}

	@PutMapping("{id}")
	@Operation(summary = "Werk een afspraakslot bij", description = "Werkt een bestaand afspraakslot bij op basis van id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "204", description = "Afspraakslot succesvol bijgewerkt")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateAfspraakslot(@PathVariable("id") Long id,
		@Parameter(description = "Nieuwe gegevens van het afspraakslot") @RequestBody ColonAfspraakslotDto afspraakslotDto)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		afspraakslotService.updateAfspraakslot(id, afspraakslotDto, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
	}

	@DeleteMapping("{ids}")
	@Operation(summary = "Verwijder afspraakslots", description = "Verwijdert één of meer afspraakslots op basis van komma-gescheiden ids.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "204", description = "Afspraakslots succesvol verwijderd")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteAfspraakslots(@Parameter(description = "Komma-gescheiden ids van de afspraakslots") @PathVariable("ids") String ids,
		@Parameter(description = "Voer alleen validatie uit") @RequestParam(required = false) Boolean alleenValidatie,
		@Parameter(description = "Geeft aan of er meerdere afspraakslots tegelijk verwijderd moeten worden") @RequestParam(required = false) Boolean bulk)
		throws BulkVerwijderenException, ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		var afspraakslotIds = Stream.of(ids.split(","))
			.map(String::trim)
			.map(Long::parseLong)
			.collect(Collectors.toList());

		if (Boolean.TRUE.equals(bulk))
		{
			afspraakslotService.bulkDeleteAfspraakslots(afspraakslotIds, ScreenitSession.get().getIngelogdeOrganisatieMedewerker(), alleenValidatie);
		}
		else
		{
			afspraakslotService.deleteAfspraakslot(afspraakslotIds.get(0), ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		}

		return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
	}
}
