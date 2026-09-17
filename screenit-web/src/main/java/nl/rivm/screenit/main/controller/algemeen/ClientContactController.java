package nl.rivm.screenit.main.controller.algemeen;

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

import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.algemeen.ClientContactActieDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactDto;
import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.mappers.algemeen.ClientContactMapper;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContactActieTypeFilter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ClientService;

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

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/client-contact")
public class ClientContactController
{
	private final ClientService clientService;

	private final ClientContactService clientContactService;

	private final ClientContactMapper clientContactMapper;

	@GetMapping("{clientId}")
	@Operation(summary = "Haal client contacten op",
		description = "Haal de client contacten op voor de gegeven client. Zonder 'type' worden alle contacten teruggegeven; met 'type' wordt gefilterd op contacten die dat "
			+ "actietype wel (moetVoorkomen=true, standaard) of juist niet (moetVoorkomen=false) hebben.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met client contacten"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<ClientContactDto>> getClientContacten(@PathVariable Long clientId, @RequestParam(required = false) ClientContactActieType type,
		@RequestParam(required = false, defaultValue = "true") boolean moetVoorkomen)
	{
		var client = clientService.getClientById(clientId).orElseThrow(() -> new EntityNietGevondenException("Client", clientId));
		var actieTypeFilter = type != null ? new ClientContactActieTypeFilter(type, moetVoorkomen) : null;
		var contacten = clientContactService.getClientContacten(client, actieTypeFilter);
		return ResponseEntity.ok(contacten.stream().map(clientContactMapper::clientContactNaarDto).toList());
	}

	@GetMapping("{clientId}/aantal-meldingen")
	@Operation(summary = "Haal het aantal client contacten met opmerking op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Aantal client contacten met een opmerking"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Long> getAantalMeldingen(@PathVariable Long clientId)
	{
		var aantal = clientContactService.countClientContactenMetOpmerking(clientId);
		return ResponseEntity.ok(aantal);
	}

	@PostMapping
	@Operation(summary = "Maak client contact aan")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Client contact aangemaakt"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> maakClientContact(@Parameter(description = "Gegevens van het nieuwe client contact") @RequestBody ClientContactDto dto)
	{
		var client = clientService.getClientById(dto.getClientId()).orElseThrow(() -> new EntityNietGevondenException("Client", dto.getClientId()));
		var actieTypes = Optional.ofNullable(dto.getActies()).orElseGet(List::of).stream().map(ClientContactActieDto::getType).toList();
		var contact = clientContactService.maakClientContact(client, dto.getDatumTijd(), actieTypes, dto.getNotitie(),
			ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@PutMapping("{id}/notitie")
	@Operation(summary = "Sla notitie wijzigingen aan client contact op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Notitie van client contact bijgewerkt"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> pasNotitieClientContactAan(@Parameter(description = "Gegevens van het nieuwe client contact") @RequestBody ClientContactDto dto,
		@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		clientContact.setOpmerking(dto.getNotitie());
		var contact = clientContactService.updateClientContact(clientContact, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@DeleteMapping("{id}/notitie")
	@Operation(summary = "Verwijder alleen de notitie van een client contact")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Notitie van client contact verwijderd"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> verwijderNotitie(@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		var contact = clientContactService.verwijderNotitie(clientContact, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@DeleteMapping("{id}")
	@Operation(summary = "Verwijder client contact")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Client contact verwijderd"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> verwijderClientContact(@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		clientContactService.verwijderContact(clientContact, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok().build();
	}

	private ClientContact getClientContactOfGooiNotFoundException(Long id)
	{
		return clientContactService.getClientContactById(id).orElseThrow(() -> new EntityNietGevondenException("Client contact", id));
	}
}
