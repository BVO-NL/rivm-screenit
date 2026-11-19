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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.mamma.MammaUitstelDto;
import nl.rivm.screenit.main.mappers.mamma.MammaUitstelMapper;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;

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
@RequestMapping("/mamma/uitstel")
@Tag(name = "Mamma uitstel", description = "Beheer van mamma-uitstel")
public class MammaUitstelController extends BaseController
{
	private final MammaBaseUitstelService baseUitstelService;

	private final MammaBaseStandplaatsService baseStandplaatsService;

	private final MammaUitstelMapper uitstelMapper;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal actief mamma-uitstel op", description = "Haalt het actieve uitstel op waarvoor nog geen nieuwe uitnodiging is verstuurd.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Mamma-uitstel opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/actief")
	public ResponseEntity<MammaUitstelDto> getActiefUitstel(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var uitstel = baseUitstelService.findActiefUitstel(client)
			.map(actiefUitstel -> uitstelMapper.uitstelNaarDto(actiefUitstel,
				baseStandplaatsService.getStandplaatsLocatie(actiefUitstel.getStandplaats(), actiefUitstel.getStreefDatum())))
			.orElse(null);
		return ResponseEntity.ok(uitstel);
	}
}
