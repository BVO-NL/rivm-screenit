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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.algemeen.HandleidingDto;
import nl.rivm.screenit.main.dto.algemeen.HandleidingUploadResultDto;
import nl.rivm.screenit.main.service.algemeen.HandleidingService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/handleiding")
@Tag(name = "Handleidingen", description = "Beheer van handleidingen in het medewerkerportaal")
public class HandleidingController
{
	private HandleidingService handleidingService;

	@GetMapping
	@Operation(summary = "Haal handleidingen op", description = "Geeft alle beschikbare handleidingen terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Handleidingen succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<HandleidingDto>> getHandleidingen()
	{
		var handleidingen = handleidingService.getHandleidingen();

		return ResponseEntity.ok(handleidingen);
	}

	@PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Upload handleidingen", description = "Upload één of meer handleidingen met de bijbehorende bestandsnamen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Handleidingen succesvol verwerkt"),
		@ApiResponse(responseCode = "207", description = "Niet alle uploads zijn geslaagd"),
		@ApiResponse(responseCode = "413", description = "Bestand is te groot om te uploaden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<HandleidingUploadResultDto>> uploadHandleidingen(
		@Parameter(description = "Bestanden die geüpload moeten worden") @RequestParam List<MultipartFile> bestanden,
		@Parameter(description = "Bestandsnamen die bij de uploads horen") @RequestParam List<String> bestandsnamen)
	{
		var resultaten = handleidingService.uploadHandleidingen(bestanden, bestandsnamen);
		var nietGeslaagdeUploads = resultaten.stream().anyMatch(r -> !r.isGeslaagd());
		if (nietGeslaagdeUploads)
		{
			return ResponseEntity.status(HttpStatus.MULTI_STATUS).body(resultaten);
		}
		return ResponseEntity.ok(resultaten);
	}

	@PutMapping(value = "{id}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Bewerk een handleiding", description = "Werkt een bestaande handleiding bij op basis van id, bestand en/of bestandsnaam.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Handleiding succesvol bijgewerkt"),
		@ApiResponse(responseCode = "400", description = "Ongeldig bestandstype opgegeven"),
		@ApiResponse(responseCode = "404", description = "Handleiding niet gevonden"),
		@ApiResponse(responseCode = "409", description = "Bestandsnaam is al in gebruik"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<HandleidingUploadResultDto> bewerkHandleiding(
		@PathVariable Long id,
		@Parameter(description = "Nieuw bestand voor de handleiding") @RequestParam(required = false) MultipartFile bestand,
		@Parameter(description = "Nieuwe bestandsnaam voor de handleiding") @RequestParam(required = false) String bestandsnaam)
	{
		var resultaat = handleidingService.bewerkHandleiding(id, bestand, bestandsnaam);
		return ResponseEntity.ok(resultaat);
	}

	@DeleteMapping(value = "{id}")
	@Operation(summary = "Verwijder een handleiding", description = "Verwijdert een handleiding op basis van het opgegeven id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Handleiding succesvol verwijderd"),
		@ApiResponse(responseCode = "404", description = "Handleiding niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<HandleidingUploadResultDto> verwijderHandleiding(
		@PathVariable Long id)
	{
		var resultaat = handleidingService.verwijderHandleiding(id);
		return ResponseEntity.ok(resultaat);
	}
}
