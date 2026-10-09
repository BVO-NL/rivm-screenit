package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.AbstractCervixProjectGroepWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixOudeNietIngestuurdeZasWriter extends AbstractCervixProjectGroepWriter<Client>
{
	private final BaseProjectService projectService;

	private final LogService logService;

	@Override
	protected void write(Client client) throws Exception
	{
		var projectGroep = prepareProjectGroep();
		var logEvent = new LogEvent();
		try
		{
			LOG.info("Voeg client (id: " + client.getId() + ") toe aan projectgroep '" + projectGroep.getNaam() + "'");
			projectService.addClientToProjectGroep(projectGroep, client);
		}
		catch (IllegalStateException e)
		{
			var melding = e.getMessage() + ". Was niet mogelijk om toe te voegen aan project.";
			LOG.warn(melding);
			logEvent.setLevel(Level.WARNING);
			logEvent.setMelding(melding);
		}
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_CLIENT_OUDE_NIET_INGESTUURDE_ZAS_HERINNERING, logEvent, client, Bevolkingsonderzoek.CERVIX);
	}
}
