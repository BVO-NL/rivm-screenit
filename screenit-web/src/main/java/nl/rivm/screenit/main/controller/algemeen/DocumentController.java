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

import java.io.FileNotFoundException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/document")
@Tag(name = "Documenten", description = "Downloaden van documenten uit het medewerkerportaal")
public class DocumentController extends BaseController
{
	private final UploadDocumentService uploadDocumentService;

	private final LogService logService;

	@GetMapping("{id}")
	@Operation(summary = "Download een document", description = "Levert een documentbestand terug op basis van het document-id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Document succesvol gedownload"),
		@ApiResponse(responseCode = "404", description = "Document niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {}, altijdToegestaan = true, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Resource> getDocument(@PathVariable Long id)
	{
		var document = uploadDocumentService.getById(id).orElse(null);
		if (document == null)
		{
			return ResponseEntity.notFound().build();
		}
		var file = uploadDocumentService.load(document);
		try
		{
			var bytes = Files.readAllBytes(file.toPath());
			var resource = new ByteArrayResource(bytes);

			var headers = new HttpHeaders();
			headers.setContentDisposition(ContentDisposition.builder("attachment").name(file.getName()).build());

			if (file.getPath().contains(FileStoreLocation.ALGEMEEN_HANDLEIDINGEN.getPath()))
			{
				logService.logGebeurtenis(LogGebeurtenis.HANDLEIDING_DOWNLOAD, getIngelogdeGebruiker(), document.getNaam() + " gedownload",
					Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA);
			}
			return ResponseEntity.ok()
				.headers(headers)
				.contentType(MediaType.valueOf(document.getContentType()))
				.contentLength(file.length())
				.body(resource);
		}
		catch (FileNotFoundException | NoSuchFileException exception)
		{
			return ResponseEntity.notFound().build();
		}
		catch (Exception exception)
		{
			LOG.error("Fout bij het download van document met id '{}'", id, exception);
			return ResponseEntity.internalServerError().build();
		}
	}
}
