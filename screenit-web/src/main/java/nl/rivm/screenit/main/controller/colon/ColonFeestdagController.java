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

import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonFeestdagService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonFeestdag;
import nl.rivm.screenit.model.colon.dto.ColonFeestdagDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

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
@RestController
@AllArgsConstructor
@RequestMapping("/colon/feestdag")
@Tag(name = "Colon feestdagen", description = "Beheer van feestdagen voor het colon-programma")
public class ColonFeestdagController
{
	private final ColonFeestdagService feestdagService;

	@GetMapping
	@Operation(summary = "Haal feestdagen op", description = "Geeft feestdagen terug, optioneel gefilterd op start- en einddatum.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Feestdagen succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.COLON_FEESTDAGEN_BEHEER,
		Recht.MEDEWERKER_LOCATIE_ROOSTER }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<List<ColonFeestdag>> getFeestdagen(
		@Parameter(description = "Optionele startdatum van de selectie") @RequestParam Optional<String> startDatum,
		@Parameter(description = "Optionele einddatum van de selectie") @RequestParam Optional<String> eindDatum)
	{
		var feestdagen = startDatum.isPresent() && eindDatum.isPresent() ? feestdagService.getFeestdagen(startDatum.get(), eindDatum.get()) : feestdagService.getFeestdagen();
		return ResponseEntity.ok(feestdagen);
	}

	@PostMapping
	@Operation(summary = "Maak een feestdag aan", description = "Maakt een nieuwe feestdag aan voor het colon-programma.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Feestdag succesvol aangemaakt"),
		@ApiResponse(responseCode = "422", description = "Validatiefout opgetreden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonFeestdag> createFeestdag(@Parameter(description = "Gegevens van de aan te maken feestdag") @RequestBody ColonFeestdagDto feestdagDto)
		throws ValidatieException
	{
		var nieuweFeestdag = feestdagService.createFeestdag(feestdagDto);
		return ResponseEntity.ok(nieuweFeestdag);
	}

	@PutMapping("{id}")
	@Operation(summary = "Werk een feestdag bij", description = "Werkt een bestaande feestdag bij op basis van id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Feestdag succesvol bijgewerkt"),
		@ApiResponse(responseCode = "422", description = "Validatiefout opgetreden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonFeestdag> updateFeestdag(@Parameter(description = "Id van de feestdag") @PathVariable long id,
		@Parameter(description = "Nieuwe gegevens van de feestdag") @RequestBody ColonFeestdagDto feestdagDto) throws ValidatieException
	{
		var updatedFeestdag = feestdagService.updateFeestdag(id, feestdagDto);
		return ResponseEntity.ok(updatedFeestdag);
	}

	@DeleteMapping("{id}")
	@Operation(summary = "Verwijder een feestdag", description = "Verwijdert een feestdag op basis van het opgegeven id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Feestdag succesvol verwijderd"),
		@ApiResponse(responseCode = "422", description = "Validatiefout opgetreden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_FEESTDAGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteFeestdag(@Parameter(description = "Id van de te verwijderen feestdag") @PathVariable long id) throws ValidatieException
	{
		feestdagService.deleteFeestdag(id);
		return ResponseEntity.ok().build();
	}
}
