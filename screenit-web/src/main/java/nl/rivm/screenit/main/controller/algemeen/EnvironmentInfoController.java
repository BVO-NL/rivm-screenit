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
import nl.rivm.screenit.main.dto.algemeen.EnvironmentInfoDto;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/environment-info")
@Tag(name = "Environment info", description = "Opvragen van omgevingsinformatie, zoals de huidige datum en tijd van de server")
public class EnvironmentInfoController extends BaseController
{
	private final ICurrentDateSupplier currentDateSupplier;

	@GetMapping
	@Operation(summary = "Haal omgevingsinformatie op", description = "Geeft de huidige datum en tijd van de server terug. Deze kan in testomgevingen via het testportaal verschoven zijn.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Omgevingsinformatie succesvol opgehaald")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {}, altijdToegestaan = true, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<EnvironmentInfoDto> getEnvironmentInfo()
	{
		return ResponseEntity.ok(new EnvironmentInfoDto(currentDateSupplier.getLocalDateTime()));
	}
}
