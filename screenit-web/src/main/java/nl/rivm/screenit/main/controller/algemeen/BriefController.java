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

import java.nio.file.Files;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.model.BriefActie;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/brief")
public class BriefController
{
	private final BriefService briefService;

	private final BaseBriefService baseBriefService;

	private final UploadDocumentService uploadDocumentService;

	private final AsposeService asposeService;

	@GetMapping("/{briefType}/{id}/acties")
	@Operation(summary = "Haal brief acties op", description = "Zoek alle toegestane acties voor een gegeven brief op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Acties die op de brief uitgevoerd mogen worden"),
		@ApiResponse(responseCode = "404", description = "Brief niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN,
		Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<BriefActie>> getBriefActies(@PathVariable Long id, @PathVariable String briefType)
	{
		var brief = getBriefOfGooiNotFoundException(id, briefType);
		var acties = briefService.getBriefActies(brief);
		return ResponseEntity.ok(acties);
	}

	@PostMapping("/{briefType}/{id}/activeren")
	@Operation(summary = "Activeer de brief", description = "Hou de brief niet meer tegen")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200"),
		@ApiResponse(responseCode = "404", description = "Brief niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht =
		Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> activeren(@PathVariable Long id, @PathVariable String briefType)
	{
		var brief = getBriefOfGooiNotFoundException(id, briefType);
		baseBriefService.briefNietMeerTegenhouden(brief, ScreenitSession.get().getIngelogdAccount());
		return ResponseEntity.ok().build();
	}

	@PostMapping("/{briefType}/{id}/tegenhouden")
	@Operation(summary = "Houd een brief tegen", description = "Zet een brief op tegenhouden zodat deze niet aangemaakt wordt")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Brief wordt tegengehouden"),
		@ApiResponse(responseCode = "404", description = "Brief niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = {
		Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> tegenhouden(@PathVariable Long id, @PathVariable String briefType)
	{
		var brief = getBriefOfGooiNotFoundException(id, briefType);
		baseBriefService.briefTegenhouden(brief, ScreenitSession.get().getIngelogdAccount());
		return ResponseEntity.ok().build();
	}

	@GetMapping("/{briefType}/{id}/template-inzien")
	@Operation(summary = "Bekijk de template van de brief", description = "Haalt het document van de template waarop de brief is gebaseerd")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Document van de template"),
		@ApiResponse(responseCode = "404", description = "Brief niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {}, altijdToegestaan = true, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Resource> templateInzien(@PathVariable Long id, @PathVariable String briefType)
	{
		var brief = getBriefOfGooiNotFoundException(id, briefType);
		try
		{
			var pdf = baseBriefService.maakPdfVanUploadDocument(brief.getBriefDefinitie().getDocument());
			var bytes = Files.readAllBytes(pdf.toPath());
			var resource = new ByteArrayResource(bytes);
			var contentType = Files.probeContentType(pdf.toPath());
			if (contentType == null)
			{
				contentType = MediaType.APPLICATION_PDF_VALUE;
			}

			var headers = new HttpHeaders();
			headers.setContentDisposition(ContentDisposition.builder("attachment").name("brieftemplate_inzien.pdf").build());

			return ResponseEntity.ok()
				.headers(headers)
				.contentType(MediaType.valueOf(contentType))
				.contentLength(pdf.length())
				.body(resource);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return ResponseEntity.internalServerError().build();
		}

	}

	private ClientBrief<?, ?, ?> getBriefOfGooiNotFoundException(Long briefId, String briefType)
	{
		return briefService.getBriefById(briefId, briefType).orElseThrow(() -> new EntityNietGevondenException("Brief", briefId));
	}
}
