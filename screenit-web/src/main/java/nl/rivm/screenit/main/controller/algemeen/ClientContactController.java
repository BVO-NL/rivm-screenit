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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.BezwaarVerwerkenDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactActieDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactDto;
import nl.rivm.screenit.main.mappers.algemeen.BezwaarMapper;
import nl.rivm.screenit.main.mappers.algemeen.ClientContactMapper;
import nl.rivm.screenit.main.service.algemeen.BezwaarService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContactActieTypeFilter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientContactService;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/client-contact")
public class ClientContactController extends BaseController
{
	private final ClientContactService clientContactService;

	private final ClientContactMapper clientContactMapper;

	private final BezwaarService bezwaarService;

	private final BezwaarMapper bezwaarMapper;

	@GetMapping("{clientId}")
	@Operation(summary = "Haal client contacten op",
		description = "Haal de client contacten op voor de gegeven client. Zonder 'type' worden alle contacten teruggegeven; met 'type' wordt gefilterd op contacten die dat "
			+ "actietype wel (moetVoorkomen=true, standaard) of juist niet (moetVoorkomen=false) hebben.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met client contacten")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<ClientContactDto>> getClientContacten(@PathVariable Long clientId, @RequestParam(required = false) ClientContactActieType type,
		@RequestParam(required = false, defaultValue = "true") boolean moetVoorkomen)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var actieTypeFilter = type != null ? new ClientContactActieTypeFilter(type, moetVoorkomen) : null;
		var contacten = clientContactService.getClientContacten(client, actieTypeFilter);
		return ResponseEntity.ok(contacten.stream().map(clientContactMapper::clientContactNaarDto).toList());
	}

	@GetMapping("{clientId}/aantal-meldingen")
	@Operation(summary = "Haal het aantal client contacten met opmerking op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Aantal client contacten met een opmerking")
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
		@ApiResponse(responseCode = "200", description = "Client contact aangemaakt")
	})
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> maakClientContact(@Parameter(description = "Gegevens van het nieuwe client contact") @RequestBody ClientContactDto dto)
	{
		var client = getClientOfGooiNotFoundException(dto.getClientId());
		var actieTypes = Optional.ofNullable(dto.getActies()).orElseGet(List::of).stream().map(ClientContactActieDto::getType).toList();
		var contact = clientContactService.maakClientContact(client, dto.getDatumTijd(), actieTypes, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@PutMapping("{id}/notitie")
	@Operation(summary = "Sla notitie wijzigingen aan client contact op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Notitie van client contact bijgewerkt"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> pasNotitieClientContactAan(@Parameter(description = "Gegevens van het nieuwe client contact") @RequestBody ClientContactDto dto,
		@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		clientContact.setOpmerking(dto.getNotitie());
		var contact = clientContactService.updateClientContact(clientContact, getIngelogdeGebruiker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@DeleteMapping("{id}/notitie")
	@Operation(summary = "Verwijder alleen de notitie van een client contact")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Notitie van client contact verwijderd"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<ClientContactDto> verwijderNotitie(@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		var contact = clientContactService.verwijderNotitie(clientContact, getIngelogdeGebruiker());
		return ResponseEntity.ok(clientContactMapper.clientContactNaarDto(contact));
	}

	@DeleteMapping("{id}")
	@Operation(summary = "Verwijder client contact")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Client contact verwijderd"),
		@ApiResponse(responseCode = "404", description = "Client contact niet gevonden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_CONTACT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> verwijderClientContact(@PathVariable Long id)
	{
		var clientContact = getClientContactOfGooiNotFoundException(id);
		clientContactService.verwijderContact(clientContact, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@PostMapping(value = "{clientId}/bezwaar-verwerken", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Verwerk de keuzes gebruik gegevens direct",
		description = "Legt een client contact met een bezwaar vast en rondt direct een nieuw bezwaarmoment af met de meegegeven bezwaren en het ondertekende formulier.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bezwaarmoment verwerkt"),
		@ApiResponse(responseCode = "400", description = "Geen of een ongeldig bestand, niet toegestane bezwaren of bezwaren niet gewijzigd"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_BEZWAAR }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> verwerkBezwaar(@PathVariable Long clientId,
		@Parameter(description = "De bezwaren die na het verwerken actief moeten zijn") @RequestPart("gegevens") BezwaarVerwerkenDto dto,
		@Parameter(description = "Het ondertekende bezwaarformulier (PDF)") @RequestPart(value = "bestand", required = false) MultipartFile bestand) throws IOException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var bezwaren = dto.getBezwaren().stream().map(bezwaarMapper::dtoToBezwaar).toList();
		bezwaarService.bezwaarDirectVerwerken(client, bezwaren, bestand, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@PostMapping(value = "{clientId}/gegevens-verwijderen", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Verwijder gegevens op verzoek van de client",
		description = "Legt een client contact met een bezwaar vast en verwijdert, afhankelijk van de gekozen bezwaren, de dossiers per bevolkingsonderzoek of alle "
			+ "gegevens (bezwaar BRP) met het ondertekende formulier.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Gegevens verwijderd"),
		@ApiResponse(responseCode = "400", description = "Geen of een ongeldig bestand, niet toegestane bezwaren of niets om te verwijderen"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = { Recht.CLIENT_DOSSIER_VERWIJDEREN,
		Recht.MEDEWERKER_BEZWAAR_BRP }, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> verwijderGegevens(@PathVariable Long clientId,
		@Parameter(description = "De te verwijderen gegevens als bezwaren") @RequestPart("gegevens") BezwaarVerwerkenDto dto,
		@Parameter(description = "Het ondertekende bezwaarformulier (PDF)") @RequestPart(value = "bestand", required = false) MultipartFile bestand) throws IOException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (dto.getBezwaren().isEmpty())
		{
			return ResponseEntity.badRequest().build();
		}
		var bezwaren = dto.getBezwaren().stream().map(bezwaarMapper::dtoToBezwaar).toList();
		bezwaarService.gegevensVerwijderen(client, bezwaren, getToegestaneVerwijderBezwaarTypes(), bestand, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	private static List<BezwaarType> getToegestaneVerwijderBezwaarTypes()
	{
		var bezwaarTypes = new ArrayList<BezwaarType>();
		if (ScreenitSession.get().checkPermission(Recht.CLIENT_DOSSIER_VERWIJDEREN, Actie.VERWIJDEREN))
		{
			bezwaarTypes.add(BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER);
		}
		if (ScreenitSession.get().checkPermission(Recht.MEDEWERKER_BEZWAAR_BRP, Actie.VERWIJDEREN))
		{
			bezwaarTypes.add(BezwaarType.GEEN_OPNAME_UIT_BPR);
		}
		return bezwaarTypes;
	}

	private ClientContact getClientContactOfGooiNotFoundException(Long id)
	{
		return clientContactService.getClientContactById(id);
	}
}
