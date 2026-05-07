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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.mappers.algemeen.MedewerkerMapper;
import nl.rivm.screenit.main.model.algemeen.dto.MedewerkerDto;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/algemeen/medewerker")
public class MedewerkerController
{
	private final MedewerkerService medewerkerService;

	private final MedewerkerMapper medewerkerMapper;

	@GetMapping
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_FOTOBESPREKING },
		organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<List<MedewerkerDto>> getMedewerkers(@RequestParam(required = false) String recht)
	{
		var medewerkers = new ArrayList<MedewerkerDto>();
		if (recht != null)
		{
			medewerkers.addAll(medewerkerService.getActieveMedewerkersMetRecht(Recht.valueOf(recht)).stream()
				.map(medewerkerMapper::medewerkerToMedewerkerDto).toList());
		}

		return ResponseEntity.ok(medewerkers);
	}
}
