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

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.ProjectMapper;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.algemeen.dto.ProjectDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectType;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/project")
@Tag(name = "Projecten", description = "Opvragen van projecten in het medewerkerportaal")
public class ProjectController
{
	private final ProjectService projectService;

	private final ProjectMapper projectMapper;

	@GetMapping()
	@Operation(summary = "Haal projecten op", description = "Geeft alle projecten terug of filtert op een specifiek projecttype.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Projecten succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_PROJECT_OVERZICHT,
		Recht.MEDEWERKER_BRIEFPROJECT_OVERZICHT }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }, organisatieTypeScopes = { OrganisatieType.RIVM })
	public ResponseEntity<List<ProjectDto>> getProjecten(@Parameter(description = "Optioneel projecttype om op te filteren") @RequestParam(required = false) ProjectType type)
	{
		List<Project> projecten;
		if (type != null)
		{
			projecten = projectService.getProjectenVanType(type);
		}
		else
		{
			projecten = projectService.getProjecten();
		}
		var projectDtos = projecten.stream().map(projectMapper::projectToDto).toList();
		return ResponseEntity.ok().body(projectDtos);
	}
}
