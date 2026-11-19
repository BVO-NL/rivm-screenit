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

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.AanhefWijzigenDto;
import nl.rivm.screenit.main.dto.algemeen.BaseClientContactDto;
import nl.rivm.screenit.main.dto.algemeen.CervixUitstelAanvragenDto;
import nl.rivm.screenit.main.dto.algemeen.CervixUitstelDto;
import nl.rivm.screenit.main.dto.algemeen.ContactActieDto;
import nl.rivm.screenit.main.dto.algemeen.DeelnamewensenRegistrerenDto;
import nl.rivm.screenit.main.dto.algemeen.KeuzeGebruikGegevensBriefDto;
import nl.rivm.screenit.main.dto.algemeen.OpnieuwAanvragenClientgegevensDto;
import nl.rivm.screenit.main.dto.algemeen.TijdelijkAdresWijzigenDto;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.mappers.algemeen.ContactActieMapper;
import nl.rivm.screenit.main.mappers.algemeen.TijdelijkAdresMapper;
import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.main.service.cervix.CervixUitstelService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientContactService;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
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
@RequiredArgsConstructor
@RestController
@RequestMapping("/contact-acties")
@Tag(name = "Contact acties", description = "Ophalen en uitvoeren van client contact acties")
public class ContactActiesController extends BaseController
{
	private final ClientContactService contactService;

	private final ContactActieMapper contactActieMapper;

	private final TijdelijkAdresMapper tijdelijkAdresMapper;

	private final DeelnamemodusService deelnamemodusService;

	private final CervixUitstelService cervixUitstelService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal contact acties op", description = "Haalt de contact acties op die de ingelogde medewerker voor deze client mag uitvoeren.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Gevonden client contact acties"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping("/{clientId}")
	public ResponseEntity<List<ContactActieDto>> getContactActies(@PathVariable Long clientId,
		@Parameter(description = "Indien meegegeven worden alleen deze actietypes gecontroleerd, anders alle actietypes") @RequestParam(required = false) List<ClientContactActieType> actieTypes)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var beschikbareActies = contactService.getAvailableActies(client, actieTypes != null ? actieTypes : List.of());
		return ResponseEntity.ok(ClientContactActieTypeWrapper.getZichtbareActies(beschikbareActies, recht -> ScreenitSession.get().checkPermission(recht, null)).stream()
			.filter(actie -> magActieUitvoeren(actie, client))
			.map(ClientContactActieTypeWrapper::getType)
			.map(contactActieMapper::contactActieTypeNaarDto)
			.toList());
	}

	private boolean magActieUitvoeren(ClientContactActieTypeWrapper actie, Client client)
	{
		return switch (actie)
		{
			case BEZWAAR -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_BEZWAAR, Actie.AANPASSEN);
			case DEELNAMEWENSEN -> heeftNogTeRegistrerenDeelnamewens(client);
			case INZAGE_PERSOONSGEGEVENS -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS, Actie.AANPASSEN);
			case TIJDELIJK_ADRES -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_GEGEVENS, Actie.AANPASSEN);
			case OPNIEUW_AANVRAGEN_CLIENTGEGEVENS -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_GBA_AANVRAGEN, Actie.AANPASSEN);
			case CERVIX_HERAANMELDEN -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_CERVIX_HERAANMELDEN, Actie.AANPASSEN);
			case MAMMA_DOELGROEP_WIJZIGEN -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_MAMMA_DOELGROEP_WIJZIGEN, Actie.AANPASSEN);
			case CERVIX_AFMELDEN -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_CERVIX_AFMELDEN, Actie.AANPASSEN);
			case CERVIX_UITSTEL -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_UITSTEL, Actie.AANPASSEN);
			case CERVIX_HERDRUK -> ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CERVIX_CLIENT_HERDRUK, Actie.TOEVOEGEN);
			default -> true;
		};
	}

	private boolean heeftNogTeRegistrerenDeelnamewens(Client client)
	{
		var deelnamewensen = deelnamemodusService.getDeelnamewensDto(client);
		return !deelnamewensen.isDeelnamewensBk() || !deelnamewensen.isDeelnamewensBmhk();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Wijzig de aanhef", description = "Legt de gewenste aanspreekvorm van de client vast als client contact.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Aanhef gewijzigd"),
		@ApiResponse(responseCode = "400", description = "Geen of een ongeldige aanhef meegegeven"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/{clientId}/aanhef-wijzigen")
	public ResponseEntity<Void> wijzigAanhef(@PathVariable Long clientId, @RequestBody AanhefWijzigenDto dto)
	{
		if (!Aanhef.aanhefVormenClienten().contains(dto.getAanhef()))
		{
			return ResponseEntity.badRequest().build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		contactService.wijzigAanhef(client, dto.getAanhef(), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_GBA_AANVRAGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Vraag clientgegevens opnieuw aan", description = "Dient een BRP-verzoek in om de gegevens van de client opnieuw op te halen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Clientgegevens opnieuw aangevraagd"),
		@ApiResponse(responseCode = "400", description = "Geen of een ongeldige reden meegegeven"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Opnieuw aanvragen van clientgegevens is voor deze client niet beschikbaar"),
	})
	@PostMapping("/{clientId}/opnieuw-aanvragen-clientgegevens")
	public ResponseEntity<Void> vraagClientgegevensOpnieuwAan(@PathVariable Long clientId, @Valid @RequestBody OpnieuwAanvragenClientgegevensDto dto)
	{
		var toegestaneRedenen = List.of(RedenGbaVraag.ONJUIST_ADRES, RedenGbaVraag.ONJUISTE_PERSOONSGEGEVENS);
		if (!toegestaneRedenen.contains(dto.getReden()))
		{
			throw new IllegalStateException("error.reden.opnieuw.aanvragen.clientgegevens.ongeldig");
		}
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		contactService.vraagClientgegevensOpnieuwAan(client, dto.getReden(), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Wijzig het tijdelijk adres", description = "Legt het tijdelijk adres van de client vast als client contact.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Tijdelijk adres gewijzigd"),
		@ApiResponse(responseCode = "400", description = "Geen tijdelijk adres meegegeven"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/{clientId}/tijdelijk-adres-wijzigen")
	public ResponseEntity<Void> wijzigTijdelijkAdres(@PathVariable Long clientId, @RequestBody TijdelijkAdresWijzigenDto dto)
	{
		if (dto.getTijdelijkAdres() == null)
		{
			return ResponseEntity.badRequest().build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		contactService.wijzigTijdelijkAdres(client, tijdelijkAdresMapper.naarTijdelijkAdres(client, dto.getTijdelijkAdres()), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_DEELNAMEWENS_REGISTREREN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de deelnamewensen op", description = "Geeft per bevolkingsonderzoek aan of er al een expliciete deelnamewens van de client is vastgelegd.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Huidige deelnamewensen"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@GetMapping("/{clientId}/deelnamewensen")
	public ResponseEntity<DeelnamewensDto> getDeelnamewensen(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(deelnamemodusService.getDeelnamewensDto(client));
	}

	@SecurityConstraint(actie = Actie.TOEVOEGEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_DEELNAMEWENS_REGISTREREN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Registreer deelnamewensen", description = "Legt de expliciete deelnamewensen van de client vast als client contact.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Deelnamewensen geregistreerd"),
		@ApiResponse(responseCode = "400", description = "Geen of geen nieuwe deelnamewens meegegeven"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/{clientId}/deelnamewensen")
	public ResponseEntity<Void> registreerDeelnamewensen(@PathVariable Long clientId, @RequestBody DeelnamewensenRegistrerenDto dto)
	{
		if (dto.getDeelnamewensen() == null)
		{
			return ResponseEntity.badRequest().build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		if (!deelnamemodusService.heeftNieuweDeelnamewensGeselecteerd(client, dto.getDeelnamewensen()))
		{
			return ResponseEntity.badRequest().build();
		}
		contactService.registreerDeelnamewensen(client, dto.getDeelnamewensen(), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_BEZWAAR, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Verstuur brief keuze gebruik gegevens", description = "Registreert een client contact en vraagt de gekozen brief keuze gebruik gegevens aan.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Brief aangevraagd"),
		@ApiResponse(responseCode = "400", description = "Geen of een ongeldige briefkeuze meegegeven"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
	})
	@PostMapping("/{clientId}/keuze-gebruik-gegevens-brief")
	public ResponseEntity<Void> verstuurKeuzeGebruikGegevensBrief(@PathVariable Long clientId, @RequestBody KeuzeGebruikGegevensBriefDto dto)
	{
		if (dto.getBriefType() == null || !BriefType.CLIENT_BEZWAAR_AANVRAAG_BRIEVEN.contains(dto.getBriefType()))
		{
			return ResponseEntity.badRequest().build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		contactService.vraagKeuzeGebruikGegevensBriefAan(client, dto.getBriefType(), dto.isHandtekeningVergeten(), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.VERWIJDEREN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_CERVIX_AANMELDEN_DEELNAME_BUITEN_BVO_BMHK,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
	@Operation(summary = "Registreer deelname buiten BVO BMHK", description = "Legt als client contact vast dat de client buiten het bevolkingsonderzoek baarmoederhalskanker deelneemt.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Deelname buiten BVO BMHK geregistreerd"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Deelname buiten BVO BMHK is voor deze client niet beschikbaar"),
	})
	@PostMapping("/{clientId}/deelname-buiten-bvo-bmhk")
	public ResponseEntity<Void> registreerDeelnameBuitenBvoBmhk(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		contactService.maakClientContact(client, ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK, null, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_CERVIX_HERAANMELDEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
	@Operation(summary = "Heraanmelden baarmoederhalskanker", description = "Meldt de client opnieuw aan voor het bevolkingsonderzoek baarmoederhalskanker en legt dit vast als client contact.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Client heraangemeld voor BMHK"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Heraanmelden BMHK is voor deze client niet beschikbaar"),
	})
	@PostMapping("/{clientId}/heraanmelden-baarmoederhalskanker")
	public ResponseEntity<Void> heraanmeldenBaarmoederhalskanker(@PathVariable Long clientId, @RequestBody BaseClientContactDto dto)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_HERAANMELDEN))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		contactService.heraanmeldenCervix(client, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_UITSTEL, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX })
	@Operation(summary = "Haal het huidige uitstel baarmoederhalskanker op", description = "Geeft het lopende uitstel van de client terug, zodat dit als standaardwaarde getoond kan worden.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Huidig uitstel (type en datum zijn leeg zonder lopend uitstel)"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Uitstel BMHK is voor deze client niet beschikbaar"),
	})
	@GetMapping("/{clientId}/uitstel-baarmoederhalskanker")
	public ResponseEntity<CervixUitstelDto> getUitstelBaarmoederhalskanker(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_UITSTEL))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		return ResponseEntity.ok(cervixUitstelService.getHuidigUitstel(client));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_UITSTEL, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX })
	@Operation(summary = "Uitstel baarmoederhalskanker aanvragen", description = "Legt het (gewijzigde) uitstel vast als client contact en annuleert de ZAS-aanvraag, herinnering en openstaande brief.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Uitstel aangevraagd"),
		@ApiResponse(responseCode = "422", description = "Ongeldige reden of datum; de melding staat in het message-veld van de response"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Uitstel BMHK is voor deze client niet beschikbaar"),
	})
	@PostMapping("/{clientId}/uitstel-baarmoederhalskanker")
	public ResponseEntity<Void> vraagUitstelBaarmoederhalskankerAan(@PathVariable Long clientId, @RequestBody CervixUitstelAanvragenDto dto)
		throws ValidatieException
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_UITSTEL))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		cervixUitstelService.valideerUitstel(client, dto.getUitstelType(), dto.getDatum());
		contactService.uitstelAanvragenCervix(client, cervixUitstelService.maakUitstel(client, dto), dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.TOEVOEGEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CERVIX_CLIENT_HERDRUK,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
	@Operation(summary = "Nieuwe uitnodiging baarmoederhalskanker aanvragen", description = "Legt de aanvraag vast als client contact en maakt direct een nieuwe uitnodiging aan op basis van de laatst verstuurde uitnodiging.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Nieuwe uitnodiging aangemaakt"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Aanmaken nieuwe uitnodiging BMHK is voor deze client niet beschikbaar"),
	})
	@PostMapping("/{clientId}/nieuwe-uitnodiging-baarmoederhalskanker")
	public ResponseEntity<Void> vraagNieuweUitnodigingBaarmoederhalskankerAan(@PathVariable Long clientId, @RequestBody BaseClientContactDto dto)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_HERDRUK))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		contactService.nieuweUitnodigingAanvragenCervix(client, dto.getNotitie(), getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}
}
