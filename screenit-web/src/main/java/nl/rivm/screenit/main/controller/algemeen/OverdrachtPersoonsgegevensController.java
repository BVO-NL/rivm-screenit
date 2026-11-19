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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import jakarta.persistence.EntityNotFoundException;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.BaseClientContactDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensKeuzesDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensVerwerkenDto;
import nl.rivm.screenit.main.mappers.algemeen.OverdrachtPersoonsgegevensMapper;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientContactService;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
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
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@RequiredArgsConstructor
@RestController
@RequestMapping("/overdracht-persoonsgegevens")
@Tag(name = "Overdracht persoonsgegevens", description = "Aanvragen en verwerken van inzage/overdracht persoonsgegevens")
public class OverdrachtPersoonsgegevensController extends BaseController
{
	private static final String EXCEL_CONTENT_TYPE = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

	private final ClientContactService contactService;

	private final OverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	private final OverdrachtPersoonsgegevensMapper overdrachtPersoonsgegevensMapper;

	private final DossierService dossierService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de overdrachten persoonsgegevens van de client op", description = "Geeft de historie van inzage/overdracht persoonsgegevens van de client terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Overdrachten persoonsgegevens van de client"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping
	public ResponseEntity<List<OverdrachtPersoonsgegevensDto>> getOverdrachten(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var overdrachten = overdrachtPersoonsgegevensService.findOverdrachten(client);
		return ResponseEntity.ok(overdrachtPersoonsgegevensMapper.overdrachtPersoonsgegevensNaarDtos(overdrachten, dossierService));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Controleer openstaand verzoek inzage/overdracht persoonsgegevens", description = "Geeft aan of er voor de client al een niet afgeronde aanvraag inzage/overdracht persoonsgegevens loopt.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Of er een openstaand verzoek is"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping("/openstaand-verzoek")
	public ResponseEntity<Boolean> heeftOpenstaandVerzoek(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(overdrachtPersoonsgegevensService.heeftOpenstaandVerzoek(client));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Verstuur aanvraagformulier inzage/overdracht persoonsgegevens", description = "Registreert een client contact en verstuurt de brief voor het aanvraagformulier inzage/overdracht persoonsgegevens naar de client.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Aanvraagformulier aangevraagd"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Er loopt al een niet afgeronde aanvraag inzage/overdracht persoonsgegevens voor deze client"),
	})
	@PostMapping("/aanvraagformulier")
	public ResponseEntity<Void> verstuurAanvraagformulier(@RequestParam Long clientId, @RequestBody BaseClientContactDto dto)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (overdrachtPersoonsgegevensService.heeftOpenstaandVerzoek(client))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		contactService.vraagInzageAanvraagformulierAan(client, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de gegevens op voor het verwerken van keuzes inzage/overdracht persoonsgegevens",
		description = "Geeft de verstuurde brieven, de beschikbare overdracht via-opties en de beschikbare bevolkingsonderzoeken van de client terug van de overdracht met het opgegeven id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Gegevens voor het verwerken van de keuzes"),
		@ApiResponse(responseCode = "404", description = "Client of overdracht niet gevonden"),
	})
	@GetMapping("{id}")
	public ResponseEntity<OverdrachtPersoonsgegevensVerwerkenDto> getVerwerkenGegevens(@RequestParam Long clientId, @PathVariable Long id)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var overdracht = overdrachtPersoonsgegevensService.findOverdracht(client, id)
			.orElseThrow(() -> new EntityNotFoundException("Overdracht persoonsgegevens " + id + " niet gevonden voor client " + clientId));
		return ResponseEntity.ok(overdrachtPersoonsgegevensMapper.verwerkenGegevensNaarDto(client, overdracht));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de gegevens op voor het verwerken van keuzes van het openstaande verzoek inzage/overdracht persoonsgegevens",
		description = "Geeft de verstuurde brieven, de beschikbare overdracht via-opties en de beschikbare bevolkingsonderzoeken van de client terug van het openstaande verzoek.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Gegevens voor het verwerken van de keuzes"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping("/openstaand-verzoek/verwerken")
	public ResponseEntity<OverdrachtPersoonsgegevensVerwerkenDto> getOpenstaandVerzoekVerwerkenGegevens(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client).orElse(null);
		return ResponseEntity.ok(overdrachtPersoonsgegevensMapper.verwerkenGegevensNaarDto(client, openstaandVerzoek));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Verwerk de keuzes van de client via het cliëntportaal",
		description = "Legt een client contact vast en registreert de keuzes van de client die via het cliëntportaal zijn doorgegeven. De overdracht is direct afgerond.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Keuzes verwerkt"),
		@ApiResponse(responseCode = "400", description = "Geen keuze gemaakt of een keuze voor een bevolkingsonderzoek zonder dossier"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping(value = "/verwerken/clientportaal", consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Void> verwerkKeuzesViaClientportaal(@RequestParam Long clientId, @RequestBody OverdrachtPersoonsgegevensKeuzesDto dto) throws IOException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!isGeldigeKeuze(client, dto))
		{
			return ResponseEntity.badRequest().build();
		}
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client).filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF);
		if (openstaandVerzoek.isPresent())
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}

		overdrachtPersoonsgegevensService.verwerkKeuzesViaClientportaal(client, overdrachtPersoonsgegevensMapper.keuzesNaarOverdrachtPersoonsgegevens(dto),
			getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Verwerk de keuzes van de client via de medewerker",
		description = "Slaat de keuzes en het ontvangen aanvraagformulier (PDF) op bij het openstaande verzoek inzage/overdracht persoonsgegevens en legt een client contact vast.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Keuzes verwerkt"),
		@ApiResponse(responseCode = "400", description = "Geen openstaand verzoek in de status aangevraagd, geen keuze, ongeldig bestand of een keuze voor een bevolkingsonderzoek zonder dossier"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping(value = "/verwerken/formulier", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	public ResponseEntity<Void> verwerkKeuzesViaMedewerker(@RequestParam Long clientId,
		@Parameter(description = "De keuzes van de client") @RequestPart("gegevens") OverdrachtPersoonsgegevensKeuzesDto dto,
		@Parameter(description = "Het ondertekende aanvraagformulier (PDF), altijd verplicht") @RequestPart(value = "bestand", required = false) MultipartFile bestand)
		throws IOException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client)
			.filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF || verzoek.getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN);
		if (openstaandVerzoek.isEmpty() || !isGeldigeKeuze(client, dto) || !isGeldigFormulier(bestand))
		{
			return ResponseEntity.badRequest().build();
		}
		overdrachtPersoonsgegevensService.verwerkKeuzesViaMedewerker(openstaandVerzoek.get(), overdrachtPersoonsgegevensMapper.keuzesNaarOverdrachtPersoonsgegevens(dto), bestand,
			getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Vraag een brief zonder handtekening aan", description = "Verstuurt eenmalig de brief voor het aanvraagformulier zonder handtekening bij het openstaande verzoek.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Brief zonder handtekening aangevraagd"),
		@ApiResponse(responseCode = "400", description = "Geen openstaand verzoek waarvoor een brief zonder handtekening kan worden aangevraagd"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/geen-handtekening")
	public ResponseEntity<Void> verstuurGeenHandtekeningBrief(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client)
			.filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF && verzoek.getGeenHandtekeningBrief() == null);
		if (openstaandVerzoek.isEmpty())
		{
			return ResponseEntity.badRequest().build();
		}
		overdrachtPersoonsgegevensService.verstuurGeenHandtekeningBrief(openstaandVerzoek.get(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Rond het openstaande verzoek inzage/overdracht persoonsgegevens af",
		description = "Zet het openstaande verzoek met het ontvangen aanvraagformulier van status brief ontvangen naar verwerkt.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Verzoek afgerond"),
		@ApiResponse(responseCode = "400", description = "Geen openstaand verzoek in de status brief ontvangen"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/afronden")
	public ResponseEntity<Void> rondOverdrachtAf(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client)
			.filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN);
		if (openstaandVerzoek.isEmpty())
		{
			return ResponseEntity.badRequest().build();
		}
		overdrachtPersoonsgegevensService.afronden(openstaandVerzoek.get());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de gegevens op die als keuze zijn vastgelegd bij het openstaande verzoek",
		description = "Geeft een Excel-bestand terug met de gegevens van de client voor de keuzes van het openstaande verzoek in de status brief ontvangen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Excel-bestand met de gegevens"),
		@ApiResponse(responseCode = "400", description = "Geen openstaand verzoek in de status brief ontvangen"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping("/gegevens-ophalen")
	public ResponseEntity<Resource> haalGegevensOp(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client)
			.filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN);
		if (openstaandVerzoek.isEmpty())
		{
			return ResponseEntity.badRequest().build();
		}
		var outputStream = new ByteArrayOutputStream();
		overdrachtPersoonsgegevensService.maakExcel(openstaandVerzoek.get(), outputStream, getIngelogdeGebruiker());

		var headers = new HttpHeaders();
		headers.setContentDisposition(ContentDisposition.attachment().filename("overdracht-persoonsgegevens.xlsx").build());
		return ResponseEntity.ok()
			.headers(headers)
			.contentType(MediaType.parseMediaType(EXCEL_CONTENT_TYPE))
			.body(new ByteArrayResource(outputStream.toByteArray()));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.VERVANGEN_DOCUMENTEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Vervang het ontvangen aanvraagformulier van het openstaande verzoek",
		description = "Vervangt het ontvangen aanvraagformulier (PDF) bij het openstaande verzoek in de status brief ontvangen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Formulier vervangen"),
		@ApiResponse(responseCode = "400", description = "Geen openstaand verzoek in de status brief ontvangen of ongeldig bestand"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping(value = "/vervang-formulier", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	public ResponseEntity<Void> vervangFormulier(@RequestParam Long clientId, @RequestPart("bestand") MultipartFile bestand) throws IOException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var openstaandVerzoek = overdrachtPersoonsgegevensService.findOpenstaandVerzoek(client)
			.filter(verzoek -> verzoek.getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN);
		if (openstaandVerzoek.isEmpty() || !isPdf(bestand))
		{
			return ResponseEntity.badRequest().build();
		}
		overdrachtPersoonsgegevensService.vervangFormulier(openstaandVerzoek.get(), bestand, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	private static boolean isGeldigeKeuze(Client client, OverdrachtPersoonsgegevensKeuzesDto dto)
	{
		var heeftKeuze = dto.isBkGegevens() || dto.isBkBeelden() || dto.isBmhkGegevens() || dto.isDkGegevens();
		var heeftDossiersVoorKeuzes = (!(dto.isBkGegevens() || dto.isBkBeelden()) || client.getMammaDossier() != null)
			&& (!dto.isBmhkGegevens() || client.getCervixDossier() != null)
			&& (!dto.isDkGegevens() || client.getColonDossier() != null);
		return heeftKeuze && heeftDossiersVoorKeuzes;
	}

	private static boolean isGeldigFormulier(MultipartFile bestand) throws IOException
	{
		return bestand != null && isPdf(bestand);
	}

	private static boolean isPdf(MultipartFile bestand) throws IOException
	{
		return !bestand.isEmpty() && FileType.PDF.isGeldigBestand(bestand.getOriginalFilename(), bestand.getContentType(), bestand.getBytes());
	}

}
