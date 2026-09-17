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

import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.AfspraakActie;
import nl.rivm.screenit.main.dto.algemeen.AfspraakDto;
import nl.rivm.screenit.main.service.mamma.MammaClientAfspraakService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/mamma/afspraak")
@Tag(name = "Mamma afspraken", description = "Beheer van mammaafspraken")
public class MammaClientAfspraakController
{
	private final MammaClientAfspraakService mammaClientAfspraakService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal mamma-afspraken op", description = "Haalt de geplande mamma-afspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Mamma-afspraken opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping
	public ResponseEntity<List<AfspraakDto>> getAfspraken(@RequestParam Long clientId)
	{
		return ResponseEntity.ok(mammaClientAfspraakService.getAfspraken(clientId));
	}

	@Operation(summary = "Haal afspraakacties op", description = "Haalt de toegestane acties voor mamma-afspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakacties opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/acties")
	public ResponseEntity<List<AfspraakActie>> getAfspraakActies(@RequestParam Long clientId)
	{
		return ResponseEntity.ok(mammaClientAfspraakService.getAfspraakActies(clientId));
	}
}
