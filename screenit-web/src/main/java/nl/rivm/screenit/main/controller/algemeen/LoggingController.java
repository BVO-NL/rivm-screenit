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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.LoggingDto;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.LogService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/logging")
@Tag(name = "Logging", description = "Loggen van gebeurtenissen vanuit het medewerkerportaal")
public class LoggingController extends BaseController
{
	private final LogService logService;

	private final ClientRepository clientRepository;

	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {}, altijdToegestaan = true, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Log een gebeurtenis", description = "Registreert een logregel voor de opgegeven gebeurtenis voor de ingelogde medewerker.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Gebeurtenis succesvol gelogd"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden tijdens het loggen")
	})
	@PostMapping
	public ResponseEntity<Void> logGebeurtenis(@RequestBody LoggingDto loggingDto)
	{
		var client = loggingDto.getClientId() == null ? null : clientRepository.getReferenceById(loggingDto.getClientId());
		var bevolkingsonderzoeken = loggingDto.getBevolkingsonderzoeken() == null
			? new Bevolkingsonderzoek[0]
			: loggingDto.getBevolkingsonderzoeken().toArray(Bevolkingsonderzoek[]::new);
		logService.logGebeurtenis(loggingDto.getLogGebeurtenis(), getIngelogdeGebruiker(), client,
			loggingDto.getOmschrijving(), bevolkingsonderzoeken);
		return ResponseEntity.ok().build();
	}
}
