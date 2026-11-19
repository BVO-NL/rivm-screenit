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

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.AfspraakActie;
import nl.rivm.screenit.main.dto.mamma.MammaAfspraakDto;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
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
public class MammaClientAfspraakController extends BaseController
{
	private final MammaAfspraakService afspraakService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal geplande mamma-afspraak op", description = "Haalt de geplande mamma-afspraak op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Mamma-afspraak opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/gepland")
	public ResponseEntity<MammaAfspraakDto> getGeplandeAfspraak(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(afspraakService.getGeplandeAfspraak(client));
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal afspraakacties op", description = "Haalt de toegestane acties voor mamma-afspraken op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Afspraakacties opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/acties")
	public ResponseEntity<List<AfspraakActie>> getAfspraakActies(@RequestParam Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(afspraakService.getAfspraakActies(client));
	}

	@SecurityConstraint(actie = Actie.TOEVOEGEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_MAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Maak bevestigingsbrief aan", description = "Maakt een bevestigingsbrief aan voor de geplande mamma-afspraak.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bevestigingsbrief aangemaakt"),
		@ApiResponse(responseCode = "403", description = "Geen recht om de afspraak te wijzigen"),
		@ApiResponse(responseCode = "404", description = "Client of geplande afspraak niet gevonden"),
		@ApiResponse(responseCode = "422", description = "Er is al een bevestigingsbrief aangevraagd voor deze afspraak")
	})
	@PostMapping("/{afspraakId}/bevestiging/brief")
	public ResponseEntity<Void> maakBevestigingsbrief(@PathVariable Long afspraakId, @RequestParam Long clientId) throws ValidatieException
	{
		if (!magAfspraakWijzigen())
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}
		var afspraak = afspraakService.getOpenAfspraak(getClientOfGooiNotFoundException(clientId), afspraakId);
		afspraakService.maakBevestigingsbrief(afspraak);
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.TOEVOEGEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_MAKEN,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Verstuur bevestigingsmail", description = "Verstuurt een bevestigingsmail voor de geplande mamma-afspraak naar het opgegeven e-mailadres.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bevestigingsmail verstuurd"),
		@ApiResponse(responseCode = "403", description = "Geen recht om de afspraak te wijzigen"),
		@ApiResponse(responseCode = "404", description = "Client of geplande afspraak niet gevonden"),
		@ApiResponse(responseCode = "422", description = "Het e-mailadres is ongeldig")
	})
	@PostMapping("/{afspraakId}/bevestiging/email")
	public ResponseEntity<Void> verstuurBevestigingsmail(@PathVariable Long afspraakId, @RequestParam Long clientId, @RequestBody String emailadres)
		throws ValidatieException
	{
		if (!magAfspraakWijzigen())
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}
		var afspraak = afspraakService.getOpenAfspraak(getClientOfGooiNotFoundException(clientId), afspraakId);
		afspraakService.verstuurBevestigingsmail(afspraak, emailadres, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	private boolean magAfspraakWijzigen()
	{
		return ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN, Actie.INZIEN);
	}
}
