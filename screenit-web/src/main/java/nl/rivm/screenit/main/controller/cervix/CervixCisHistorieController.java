package nl.rivm.screenit.main.controller.cervix;

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

import nl.rivm.screenit.main.dto.cervix.CervixCisHistorieDto;
import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.service.cervix.CervixCisHistorieService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/cervix/cis-historie")
public class CervixCisHistorieController
{
	private final ClientService clientService;

	private final CervixCisHistorieService cisHistorieService;

	@GetMapping("/{clientId}")
	@Operation(summary = "Haal CIS historie op", description = "Zoek de CIS historie voor een gegeven clientId op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "CIS historie succesvol opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_CIS_HISTORIE, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.CERVIX })
	ResponseEntity<CervixCisHistorieDto> getCisHistorie(@PathVariable Long clientId)
	{
		var client = clientService.getClientById(clientId).orElseThrow(() -> new EntityNietGevondenException("Client", clientId));
		var historie = cisHistorieService.getCisHistorieByClient(client);
		return ResponseEntity.ok(historie);
	}
}
