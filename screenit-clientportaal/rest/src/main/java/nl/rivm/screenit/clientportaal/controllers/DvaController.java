package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import java.io.FileInputStream;
import java.io.FileNotFoundException;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.repository.algemeen.ClientBriefRepository;
import nl.rivm.screenit.service.BaseDvaBronService;

import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("dvabron")
@AllArgsConstructor
@Slf4j
public class DvaController
{
	private final BaseDvaBronService dvaBronService;

	private final ClientBriefRepository clientBriefRepository;

	@GetMapping("/{id}")
	public ResponseEntity<Resource> getDvaBronById(@PathVariable long id)
	{
		try
		{
			var brief = clientBriefRepository.findById(id).orElseThrow(() -> new FileNotFoundException("De clientbrief kan niet worden gevonden"));
			var file = dvaBronService.maakPgoUitslagPdfBrief(brief);
			var resource = new InputStreamResource(new FileInputStream(file));

			var headers = new HttpHeaders();
			headers.setContentDisposition(ContentDisposition.builder("attachment").filename(file.getName()).build());

			return ResponseEntity.ok()
				.headers(headers)
				.contentType(MediaType.APPLICATION_PDF)
				.contentLength(file.length())
				.body(resource);
		}
		catch (FileNotFoundException exception)
		{
			LOG.error(exception.getMessage(), exception);
			return ResponseEntity.notFound().build();
		}
		catch (Exception exception)
		{
			LOG.error("Fout bij het maken van de PDF voor brief met id {}: {}", id, exception.getMessage(), exception);
			return ResponseEntity.internalServerError().build();
		}
	}
}
