package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.Map;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.batch.repository.ProjectBriefRepository;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.BaseBriefService;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProjectBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{
	@Autowired
	private ProjectBriefRepository projectBriefRepository;

	@Autowired
	private BaseBriefService briefService;

	@Override
	protected void fillingData(Map<String, ExecutionContext> partities, ScreeningOrganisatie organisatie)
	{
		var projectBriefActies = projectBriefRepository.getActieveProjectBriefActieDefinities(briefService.isOverbruggingssituatieParagonStarted() ? null : organisatie);
		for (var projectBriefActieId : projectBriefActies)
		{
			var executionContext = new ExecutionContext();
			executionContext.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
			executionContext.put(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID, projectBriefActieId);
			partities.put(organisatie.getId() + "_" + projectBriefActieId.toString(), executionContext);
		}
	}
}
