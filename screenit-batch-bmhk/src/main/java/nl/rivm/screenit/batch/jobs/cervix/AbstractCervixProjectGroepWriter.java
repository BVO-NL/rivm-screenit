package nl.rivm.screenit.batch.jobs.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.repository.algemeen.ProjectGroepRepository;
import nl.rivm.screenit.repository.algemeen.ProjectRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class AbstractCervixProjectGroepWriter<S extends HibernateObject> extends BaseWriter<S>
{
	public static final String PROJECT_ID = "project.id.key";

	public static final String PROJECT_GROEP_ID = "project.groep.id.key";

	@Autowired
	private ProjectRepository projectRepository;

	@Autowired
	private ProjectGroepRepository projectGroepRepository;

	@Autowired
	protected ICurrentDateSupplier currentDateSupplier;

	protected ProjectGroep prepareProjectGroep()
	{
		var context = getExecutionContext();
		if (!context.containsKey(PROJECT_GROEP_ID))
		{
			var groepsNaam = "Populatie van run " + DateUtil.formatShortDate(currentDateSupplier.getDate());
			var projectId = context.getLong(PROJECT_ID);
			var project = projectRepository.getReferenceById(projectId);
			LOG.info("Maak project groep '" + groepsNaam + "' voor project '" + project.getNaam() + "'");
			var projectGroep = new ProjectGroep();
			projectGroep.setProject(project);
			project.getGroepen().add(projectGroep);
			projectGroep.setActief(true);
			projectGroep.setGroepInvoer(GroepInvoer.CRITERIA);
			projectGroep.setActiefDatum(currentDateSupplier.getDate());
			projectGroep.setNaam(groepsNaam);
			projectGroep.setPopulatie(0);
			projectGroepRepository.persist(projectGroep);
			context.put(PROJECT_GROEP_ID, projectGroep.getId());
			return projectGroep;
		}
		return projectGroepRepository.getReferenceById(context.getLong(PROJECT_GROEP_ID));
	}
}
