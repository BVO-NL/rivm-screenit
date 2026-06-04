package nl.rivm.screenit.main.controllers.algemeen;

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

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/algemeen/handleiding")
public class HandleidingController
{
	private HandleidingService handleidingService;

	@GetMapping
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<HandleidingDto>> getHandleidingen()
	{
		var handleidingen = handleidingService.getHandleidingen();

		return ResponseEntity.ok(handleidingen);
	}

	@PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<HandleidingUploadResultDto>> uploadHandleidingen(
		@RequestParam List<MultipartFile> bestanden,
		@RequestParam List<String> bestandsnamen)
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
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<HandleidingUploadResultDto> bewerkHandleiding(
		@PathVariable Long id,
		@RequestParam(required = false) MultipartFile bestand,
		@RequestParam(required = false) String bestandsnaam)
	{
		var resultaat = handleidingService.bewerkHandleiding(id, bestand, bestandsnaam);
		return ResponseEntity.ok(resultaat);
	}

	@DeleteMapping(value = "{id}")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.HANDLEIDINGEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<HandleidingUploadResultDto> verwijderHandleiding(
		@PathVariable Long id)
	{
		var resultaat = handleidingService.verwijderHandleiding(id);
		return ResponseEntity.ok(resultaat);
	}
}
