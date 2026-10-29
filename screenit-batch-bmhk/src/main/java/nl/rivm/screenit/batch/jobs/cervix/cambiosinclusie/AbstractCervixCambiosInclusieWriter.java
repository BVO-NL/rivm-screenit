package nl.rivm.screenit.batch.jobs.cervix.cambiosinclusie;

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

import nl.rivm.screenit.batch.jobs.cervix.AbstractCervixProjectGroepWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.LogService;

@Slf4j
public abstract class AbstractCervixCambiosInclusieWriter extends AbstractCervixProjectGroepWriter<Client>
{
	protected final BaseProjectService projectService;

	protected final LogService logService;

	protected AbstractCervixCambiosInclusieWriter(BaseProjectService projectService, LogService logService)
	{
		this.projectService = projectService;
		this.logService = logService;
	}

	protected abstract ProjectGroep bepaalProjectGroep();

	@Override
	protected void write(Client client) throws Exception
	{
		var projectGroep = bepaalProjectGroep();
		try
		{
			LOG.info("Voeg client (id: {}) toe aan projectgroep '{}'", client.getId(), projectGroep.getNaam());
			projectService.addClientToProjectGroep(projectGroep, client);
		}
		catch (IllegalStateException e)
		{
			var melding = e.getMessage() + ". Was niet mogelijk om client toe te voegen aan project '" + projectGroep.getProject().getNaam() + "'";
			LOG.warn(melding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CLIENT_TOEGEVOEGD_AAN_CAMBIOS_STUDIE, client, melding, Bevolkingsonderzoek.CERVIX);
		}
	}
}
