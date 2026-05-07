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

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.services.OpenstaandeUitnodigingService;
import nl.rivm.screenit.model.uitnodiging.dto.OpenstaandeUitnodigingDto;
import nl.rivm.screenit.model.uitnodiging.dto.OpenstaandeUitnodigingMeedoenRequest;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("mamma/afspraak/openstaande-uitnodiging")
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class OpenstaandeUitnodigingController extends AbstractController
{
	private final OpenstaandeUitnodigingService service;

	@GetMapping
	public ResponseEntity<List<OpenstaandeUitnodigingDto>> getOpenstaandeUitnodigingen(Authentication authentication)
	{
		var client = getClient(authentication);
		var openstaandeUitnodigingenDto = service.getOpenstaandeUitnodigingen(client);
		if (openstaandeUitnodigingenDto == null || openstaandeUitnodigingenDto.isEmpty())
		{
			return ResponseEntity.ok(List.of());
		}
		return ResponseEntity.ok(openstaandeUitnodigingenDto);
	}

	@PostMapping("/meedoen")
	public ResponseEntity<Void> meedoenAanUitnodiging(Authentication authentication, @RequestBody OpenstaandeUitnodigingMeedoenRequest request)
	{
		var client = getClient(authentication);
		if (!service.verwerkMeedoen(client, request))
		{
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
		}

		return ResponseEntity.ok().build();
	}
}
