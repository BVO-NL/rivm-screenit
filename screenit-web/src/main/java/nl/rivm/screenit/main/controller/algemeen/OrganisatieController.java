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

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.OrganisatieDto;
import nl.rivm.screenit.main.mappers.mamma.MammaScreeningsEenheidMapper;
import nl.rivm.screenit.main.model.algemeen.dto.OrganisatieZoekFilterDto;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.OrganisatieMapper;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.OrganisatieService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/algemeen/organisatie")
public class OrganisatieController
{
	private final MammaBeoordelingsEenheidService beoordelingsEenheidService;

	private final MammaScreeningsEenheidService screeningsEenheidService;

	private final OrganisatieService organisatieService;

	private final OrganisatieMapper organisatieMapper;

	private final MammaScreeningsEenheidMapper mammaScreeningsEenheidMapper;

	@GetMapping("/centrale-eenheid")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<List<OrganisatieDto>> getCentraleEenheden()
	{
		var centraleEenheden = organisatieService.getMogelijkeCentraleEenheden(ScreenitSession.get().getOrganisatie()).stream().map(organisatieMapper::organisatieToDto).toList();
		return ResponseEntity.ok(centraleEenheden);
	}

	@GetMapping("/beoordelingseenheid")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<List<OrganisatieDto>> getBeoordeelingseenheden()
	{
		var beoordelingseenheden = beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie()).stream().map(organisatieMapper::organisatieToDto)
			.toList();
		return ResponseEntity.ok(beoordelingseenheden);
	}

	@PostMapping("/beoordelingseenheid/zoeken")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<List<OrganisatieDto>> zoekBeoordelingseenheden(@RequestBody OrganisatieZoekFilterDto filter)
	{

		var beoordelingseenheden = beoordelingsEenheidService.getBeoordelingseenhedenById(ScreenitSession.get().getOrganisatie(), filter.getOrganisatieIds())
			.stream().map(organisatieMapper::organisatieToDto).toList();
		return ResponseEntity.ok(beoordelingseenheden);
	}

	@PostMapping("/screeningseenheid/zoeken")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<List<OrganisatieDto>> zoekScreeningsEenheden(@RequestBody OrganisatieZoekFilterDto filter)
	{
		var screeningseenheden = screeningsEenheidService.getActieveScreeningsEenhedenVoorBeoordelingseenheidIds(filter.getOrganisatieIds()).stream()
			.map(mammaScreeningsEenheidMapper::screeningsEenheidToOrganisatieDto).toList();
		return ResponseEntity.ok(screeningseenheden);
	}
}
