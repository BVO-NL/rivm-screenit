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

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.PagineringRequestDto;
import nl.rivm.screenit.main.dto.PagineringResponseDto;
import nl.rivm.screenit.main.dto.algemeen.AfspraakActie;
import nl.rivm.screenit.main.dto.algemeen.AfspraakDto;
import nl.rivm.screenit.main.dto.colon.ColonAfspraakAfzeggenDto;
import nl.rivm.screenit.main.dto.colon.ColonAfspraakMakenRequestDto;
import nl.rivm.screenit.main.dto.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.main.mappers.colon.ColonAfspraakMapper;
import nl.rivm.screenit.main.mappers.colon.ColonAfspraakslotMapper;
import nl.rivm.screenit.main.service.colon.ColonIntakeafspraakService;
import nl.rivm.screenit.main.util.PagineringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.service.colon.PlanningService;

import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/colon/afspraak")
@Tag(name = "Colon afspraken", description = "Beheer van colonafspraken")
public class ColonAfspraakController extends BaseController
{
	private final ColonIntakeafspraakService intakeafspraakService;

	private final PlanningService planningService;

	private final ColonAfspraakMapper afspraakMapper;

	private final OrganisatieParameterService organisatieParameterService;

	private final ColonAfspraakslotMapper afspraakslotMapper;

	private final ColonIntakelocatieService intakelocatieService;

	private final ICurrentDateSupplier currentDateSupplier;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Haal intakeafspraken op", description = "Haalt de geplande intakeafspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Intakeafspraken opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client of intakeafspraak niet gevonden")
	})
	@GetMapping
	public ResponseEntity<List<AfspraakDto>> getAfspraken(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(intakeafspraakService.getAfspraken(client));
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Zoek afspraakslots", description = "Haalt de afspraakslots op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met afspraakslots"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@PostMapping("zoeken")
	public ResponseEntity<PagineringResponseDto<List<ColonVrijSlotZonderKamerDto>>> zoekAfspraakslots(@RequestParam Long clientId,
		@RequestBody PagineringRequestDto<VrijSlotZonderKamerFilter> request)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var sortering = request.getSortering();
		var paginering = PagineringUtil.maakPageVanRequest(request.getPaginering(), sortering);
		var filter = request.getData();

		var sorteerVeld = bepaalSorteerVeld(sortering.getVeld(), filter);

		List<VrijSlotZonderKamer> slots = planningService.getVrijeSlotenZonderKamer(sorteerVeld, sortering.getRichting() == Sort.Direction.ASC, paginering.getOffset(),
			paginering.getPageSize(), filter, client);
		var totaal = (int) planningService.getVrijeSlotenZonderKamerCount(filter, client);

		var response = new PagineringResponseDto<List<ColonVrijSlotZonderKamerDto>>();
		response.setData(slots.stream().map(slot -> afspraakslotMapper.vrijSlotZonderKamerNaarDto(slot, intakelocatieService, organisatieParameterService)).toList());
		response.setPaginering(request.getPaginering());
		response.getPaginering().setTotaal(totaal);
		return ResponseEntity.ok(response);
	}

	private String bepaalSorteerVeld(String sorteerVeld, VrijSlotZonderKamerFilter filter)
	{
		if (Boolean.TRUE.equals(filter.getAlleenIntakelocaties()) && "vanaf".equals(sorteerVeld))
		{
			return "naam";
		}
		return sorteerVeld;
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Verplaats de afspraak", description = "Verplaatst de locatie en/of de tijdstip van de afspraak")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "De verplaatste afspraak"),
		@ApiResponse(responseCode = "400", description = "Niet alle gegevens zijn geselecteerd, het gekozen slot is niet meer beschikbaar, of er is geen actieve kamer beschikbaar"),
		@ApiResponse(responseCode = "401", description = "De gebruiker heeft geen rechten om de afspraak te verplaatsen"),
		@ApiResponse(responseCode = "404", description = "Client of intakelocatie niet gevonden")
	})
	@PutMapping("/{afspraakId}/verplaatsen")
	public ResponseEntity<AfspraakDto> verplaatsAfspraak(@PathVariable Long afspraakId, @RequestBody ColonAfspraakMakenRequestDto request)
	{
		var client = getClientOfGooiNotFoundException(request.getClientId());
		var spoedOfBuitenRooster = request.getDatumTijdBuitenRooster() != null || intakeafspraakService.ligtInSpoedperiode(request.getAfspraakslot());
		if (spoedOfBuitenRooster
			&& !ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_INTAKE_VERPLAATS_BINNEN_INTAKE_NIET_WIJZIGBAAR_PERIODE, Actie.AANPASSEN))
		{
			return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
		}
		var briefTegenhouden = request.getBriefType() == null
			&& (spoedOfBuitenRooster || ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN));
		var briefType = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF, Actie.AANPASSEN) ? request.getBriefType() : null;
		var account = getIngelogdeGebruiker();

		var verwezenOmMedischeRedenen = Boolean.TRUE.equals(request.getVerwezenMedischeRedenenDoorInfolijn());

		var afspraak = intakeafspraakService.verplaatsAfspraak(client, request.getAfspraakslot(), request.getDatumTijdBuitenRooster(), briefType, briefTegenhouden,
			verwezenOmMedischeRedenen, request.getNotitie(), account);

		return ResponseEntity.ok(afspraakMapper.colonAfspraakToAfspraakDto(afspraak, organisatieParameterService, currentDateSupplier));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Zeg de coloscopie-intakeafspraak af", description = "Zegt de geplande coloscopie-intakeafspraak van de client af via een clientcontact.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraak succesvol afgezegd"),
		@ApiResponse(responseCode = "403", description = "Geen recht om de bevestigingsbrief tegen te houden"),
		@ApiResponse(responseCode = "404", description = "Client of geplande intakeafspraak niet gevonden")
	})
	@PostMapping("/{afspraakId}/afzeggen")
	public ResponseEntity<Void> annuleerAfspraak(@PathVariable Long afspraakId, @RequestBody ColonAfspraakAfzeggenDto afzeggenDto)
	{
		if (afzeggenDto.briefTegenhouden() && !ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}
		intakeafspraakService.annuleerAfspraak(afspraakId, afzeggenDto.briefTegenhouden(), afzeggenDto.notitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Haal afspraakacties op", description = "Haalt de toegestane acties voor colonafspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakacties opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client of intakeafspraak niet gevonden")
	})
	@GetMapping("/acties")
	public ResponseEntity<List<AfspraakActie>> getAfspraakActies(@RequestParam Long clientId, @RequestParam(required = false) Long afspraakId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var acties = new ArrayList<>(intakeafspraakService.getAfspraakActies(client, afspraakId));
		if (!ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_NIEUWE_INTAKEAFSPRAAKGEMAAKT, null))
		{
			acties.remove(AfspraakActie.MAKEN);
		}
		return ResponseEntity.ok(acties);
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Maak nieuwe afspraak aan", description = "Maakt een nieuwe afspraak aan voor de opgegeven client")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "De gemaakte afspraak"),
		@ApiResponse(responseCode = "400", description = "Niet alle gegevens zijn geselecteerd, het gekozen slot is niet meer beschikbaar, of er is geen actieve kamer beschikbaar"),
		@ApiResponse(responseCode = "404", description = "Client of intakelocatie niet gevonden")
	})
	@PostMapping
	public ResponseEntity<AfspraakDto> maakAfspraak(@RequestBody ColonAfspraakMakenRequestDto request)
	{
		var client = getClientOfGooiNotFoundException(request.getClientId());
		var briefTegenhouden = request.getBriefType() == null;
		var briefType = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF, Actie.AANPASSEN) ? request.getBriefType() : null;
		var account = getIngelogdeGebruiker();

		var afspraak = intakeafspraakService.maakAfspraak(client, request.getAfspraakslot(), request.getDatumTijdBuitenRooster(), briefType, briefTegenhouden, request.getNotitie(),
			account);

		return ResponseEntity.ok(afspraakMapper.colonAfspraakToAfspraakDto(afspraak, organisatieParameterService, currentDateSupplier));
	}
}
