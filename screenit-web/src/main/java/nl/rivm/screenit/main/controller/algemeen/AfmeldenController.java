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
import java.util.List;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.cervix.CervixAfmeldenDto;
import nl.rivm.screenit.main.service.algemeen.BvoAfmeldenService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientContactService;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/afmelden")
@Tag(name = "Afmelden", description = "Afmelden van een client voor een bevolkingsonderzoek")
public class AfmeldenController extends BaseController
{
	private final ClientContactService contactService;

	private final BvoAfmeldenService bvoAfmeldenService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_CERVIX_AFMELDEN,
		bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
	@Operation(summary = "Haal de afmeldopties op", description = "Geeft de types afmelden terug die voor deze client mogelijk zijn.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Mogelijke types afmelden"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "501", description = "Afmelden is voor dit bevolkingsonderzoek nog niet beschikbaar"),
	})
	@GetMapping("/{clientId}/{bvo}/opties")
	public ResponseEntity<List<AfmeldingType>> getAfmeldOpties(@PathVariable Long clientId, @PathVariable Bevolkingsonderzoek bvo)
	{
		if (bvo != Bevolkingsonderzoek.CERVIX)
		{
			return ResponseEntity.status(HttpStatus.NOT_IMPLEMENTED).build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(bvoAfmeldenService.getAfmeldOpties(client, bvo));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_CERVIX_AFMELDEN,
		bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
	@Operation(summary = "Meld de client af",
		description = "Legt een client contact vast en meldt de client eenmalig of definitief af. Bij definitief en direct afmelden is het ondertekende formulier (PDF) verplicht.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Client afgemeld"),
		@ApiResponse(responseCode = "400", description = "Ongeldige afmeldgegevens, of geen of een ongeldig bestand"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Afmelden is voor deze client niet beschikbaar"),
		@ApiResponse(responseCode = "501", description = "Afmelden is voor dit bevolkingsonderzoek nog niet beschikbaar"),
	})
	@PostMapping(value = "/{clientId}/{bvo}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	public ResponseEntity<Void> afmelden(@PathVariable Long clientId, @PathVariable Bevolkingsonderzoek bvo,
		@Parameter(description = "De afmeldgegevens") @Valid @RequestPart("gegevens") CervixAfmeldenDto dto,
		@Parameter(description = "Het ondertekende afmeldformulier (PDF), alleen bij definitief en direct afmelden") @RequestPart(value = "bestand", required = false)
		MultipartFile bestand) throws IOException
	{
		if (bvo != Bevolkingsonderzoek.CERVIX)
		{
			return ResponseEntity.status(HttpStatus.NOT_IMPLEMENTED).build();
		}
		var client = getClientOfGooiNotFoundException(clientId);
		if (!contactService.getAvailableActies(client).contains(ClientContactActieType.CERVIX_AFMELDEN))
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		bvoAfmeldenService.afmelden(client, bvo, dto, bestand, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}
}
