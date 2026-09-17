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
import nl.rivm.screenit.main.dto.colon.ColonAfspraakMakenRequestDto;
import nl.rivm.screenit.main.dto.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.main.mappers.colon.ColonAfspraakMapper;
import nl.rivm.screenit.main.mappers.colon.ColonAfspraakslotMapper;
import nl.rivm.screenit.main.service.colon.ColonAfspraakService;
import nl.rivm.screenit.main.service.colon.ColonIntakeafspraakService;
import nl.rivm.screenit.main.util.PagineringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.service.colon.PlanningService;

import org.springframework.data.domain.Sort;
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
	private final ColonAfspraakService colonAfspraakService;

	private final ColonIntakeafspraakService intakeafspraakService;

	private final PlanningService planningService;

	private final ColonAfspraakMapper afspraakMapper;

	private final OrganisatieParameterService organisatieParameterService;

	private final ColonAfspraakslotMapper afspraakslotMapper;

	private final ColonIntakelocatieService intakelocatieService;

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
		return ResponseEntity.ok(colonAfspraakService.getAfspraken(client));
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

		List<VrijSlotZonderKamer> slots = planningService.getVrijeSlotenZonderKamer(sortering.getVeld(), sortering.getRichting() == Sort.Direction.ASC, paginering.getOffset(),
			paginering.getPageSize(), filter, client);
		var totaal = (int) planningService.getVrijeSlotenZonderKamerCount(filter, client);

		var response = new PagineringResponseDto<List<ColonVrijSlotZonderKamerDto>>();
		response.setData(slots.stream().map(slot -> afspraakslotMapper.vrijSlotZonderKamerNaarDto(slot, intakelocatieService)).toList());
		response.setPaginering(request.getPaginering());
		response.getPaginering().setTotaal(totaal);
		return ResponseEntity.ok(response);
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
	@Operation(summary = "Verplaats de afspraak", description = "Verplaatst de locatie en/of de tijdstip van de afspraak")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "De verplaatste afspraak"),
		@ApiResponse(responseCode = "400", description = "Niet alle gegevens zijn geselecteerd, het gekozen slot is niet meer beschikbaar, of er is geen actieve kamer beschikbaar"),
		@ApiResponse(responseCode = "404", description = "Client of intakelocatie niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Er is een interne serverfout opgetreden")
	})
	@PutMapping("/{afspraakId}/verplaatsen")
	public ResponseEntity<AfspraakDto> verplaatsAfspraak(@PathVariable Long afspraakId, @RequestBody ColonAfspraakMakenRequestDto request)
	{
		var client = getClientOfGooiNotFoundException(request.getClientId());
		var briefTegenhouden = request.getBriefType() == null;
		var briefType = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF, Actie.AANPASSEN) ? request.getBriefType() : null;
		var account = getIngelogdeGebruiker();

		var afspraak = intakeafspraakService.verplaatsAfspraak(client, request.getAfspraakslot(), null, briefType, briefTegenhouden, false, request.getNotitie(), account);

		return ResponseEntity.ok(afspraakMapper.colonAfspraakToAfspraakDto(afspraak, organisatieParameterService));
	}

	@Operation(summary = "Haal afspraakacties op", description = "Haalt de toegestane acties voor colonafspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakacties opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client of intakeafspraak niet gevonden")
	})
	@GetMapping("/acties")
	public ResponseEntity<List<AfspraakActie>> getAfspraakActies(@RequestParam Long clientId, @RequestParam(required = false) Long afspraakId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var acties = new ArrayList<>(colonAfspraakService.getAfspraakActies(client, afspraakId));
		if (!ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_NIEUWE_INTAKEAFSPRAAKGEMAAKT, Actie.TOEVOEGEN))
		{
			acties.remove(AfspraakActie.MAKEN);
		}
		return ResponseEntity.ok(acties);
	}
}
