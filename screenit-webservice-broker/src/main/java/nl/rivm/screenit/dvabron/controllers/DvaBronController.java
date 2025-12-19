package nl.rivm.screenit.dvabron.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dvabron.models.DvaToetsRequest;
import nl.rivm.screenit.dvabron.models.DvaToetsResponse;
import nl.rivm.screenit.dvabron.services.DvaBronService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("api/rest/dvabron/v1")
public class DvaBronController
{
	private final DvaBronService service;

	@PostMapping("/toets")
	public ResponseEntity<DvaToetsResponse> getToets(@Valid @RequestBody DvaToetsRequest dvaToetsRequest)
	{
		return ResponseEntity.ok(service.checkBeschikbaarheidToets(dvaToetsRequest));
	}
}
