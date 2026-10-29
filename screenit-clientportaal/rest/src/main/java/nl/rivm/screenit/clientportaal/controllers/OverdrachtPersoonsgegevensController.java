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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.mappers.OverdrachtPersoonsgegevensMapper;
import nl.rivm.screenit.clientportaal.model.OverdrachtPersoonsgegevensDto;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("overdracht-persoonsgegevens")
@Slf4j
@RequiredArgsConstructor
public class OverdrachtPersoonsgegevensController extends AbstractController
{
	private final BaseOverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	private final OverdrachtPersoonsgegevensMapper overdrachtPersoonsgegevensMapper;

	private final UploadDocumentService uploadDocumentService;

	private final ClientContactService clientContactService;

	@GetMapping
	public ResponseEntity<OverdrachtPersoonsgegevensDto> getOverdrachtPersoonsgegevens(Authentication authentication)
	{
		var client = getClient(authentication);
		if (!clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.INZAGE_PERSOONSGEGEVENS))
		{
			return createForbiddenResponse();
		}

		var persoonsgegevens = overdrachtPersoonsgegevensService.getOverdrachtPersoonsgegevensVoorClient(client);
		if (persoonsgegevens == null)
		{
			return ResponseEntity.notFound().build();
		}
		return ResponseEntity.ok(overdrachtPersoonsgegevensMapper.overdrachtPersoonsgegevensToDto(persoonsgegevens));
	}

	@GetMapping("/download")
	public ResponseEntity<Resource> getDataDump(Authentication authentication)
	{
		var client = getClient(authentication);
		if (!clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.INZAGE_PERSOONSGEGEVENS))
		{
			return createForbiddenResponse();
		}

		var overdrachtPersoonsgegevens = overdrachtPersoonsgegevensService.getOverdrachtPersoonsgegevensVoorClient(client);
		if (overdrachtPersoonsgegevens == null || overdrachtPersoonsgegevens.getGegenereerdeDataDump() == null)
		{
			return ResponseEntity.notFound().build();
		}
		var gegenereerdeDataDump = overdrachtPersoonsgegevens.getGegenereerdeDataDump();
		var file = uploadDocumentService.load(gegenereerdeDataDump);
		if (!file.exists())
		{
			return ResponseEntity.notFound().build();
		}
		try
		{
			var headers = new HttpHeaders();
			headers.setContentDisposition(ContentDisposition.builder("attachment").name(file.getName()).build());

			return ResponseEntity.ok()
				.headers(headers)
				.contentType(MediaType.valueOf(gegenereerdeDataDump.getContentType()))
				.contentLength(file.length())
				.body(new FileSystemResource(file));
		}
		catch (Exception exception)
		{
			LOG.error("Fout bij het downloaden van document met id '{}'", gegenereerdeDataDump.getId(), exception);
			return ResponseEntity.internalServerError().build();
		}
	}
}
