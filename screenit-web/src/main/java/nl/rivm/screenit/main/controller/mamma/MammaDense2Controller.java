package nl.rivm.screenit.main.controller.mamma;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.MammaDense2ConfiguratieDto;
import nl.rivm.screenit.main.service.mamma.MammaDense2Service;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
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
@RequestMapping("/mamma/dense2")
@Tag(name = "Mamma DENSE2", description = "Beheer van de DENSE2-configuratie en imports")
public class MammaDense2Controller
{
	private final MammaBaseDense2Service baseDense2Service;

	private final MammaDense2Service dense2Service;

	@GetMapping("/configuratie")
	@Operation(summary = "Haal de DENSE2-configuratie op", description = "Geeft de huidige DENSE2-configuratie terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "DENSE2-configuratie succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public MammaDense2ConfiguratieDto getConfiguratie()
	{
		return baseDense2Service.getConfiguratie();
	}

	@PutMapping("/configuratie")
	@Operation(summary = "Werk de DENSE2-configuratie bij", description = "Slaat gewijzigde DENSE2-configuratie op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "DENSE2-configuratie succesvol bijgewerkt"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> updateConfiguratie(@Parameter(description = "Nieuwe DENSE2-configuratie") @RequestBody MammaDense2ConfiguratieDto configuratie)
	{
		var organisatieMedewerker = ScreenitSession.get().getIngelogdeOrganisatieMedewerker();
		baseDense2Service.updateConfiguratie(configuratie, organisatieMedewerker);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/export")
	@Operation(summary = "Exporteer DENSE2-clienten", description = "Geeft een CSV-export van DENSE2-clientgegevens terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "DENSE2-export succesvol gegenereerd"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Resource> getExport()
	{
		var file = baseDense2Service.getExport();
		var resource = new FileSystemResource(file);
		var headers = new HttpHeaders();
		headers.add(HttpHeaders.CONTENT_TYPE, "text/csv");
		headers.add(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + file.getName());
		return ResponseEntity.ok().headers(headers).body(resource);
	}

	@PostMapping("/import")
	@Operation(summary = "Importeer DENSE2-clienten", description = "Verwerkt een aangeleverd bestand met DENSE2-clientgegevens.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "DENSE2-import succesvol uitgevoerd"),
		@ApiResponse(responseCode = "406", description = "DENSE2-import kon niet worden verwerkt"),
		@ApiResponse(responseCode = "413", description = "Bestand is te groot om te uploaden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MAMMA_DENSE_2, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<String> importClienten(@Parameter(description = "Importbestand met DENSE2-clienten") @RequestParam(value = "file") MultipartFile file)
	{
		try
		{
			var melding = dense2Service.importClienten(file, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
			return ResponseEntity.ok().body(melding);
		}
		catch (Exception ex)
		{
			LOG.error("Fout bij importeren DENSE2 populatie", ex);
			return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).build();
		}
	}
}
