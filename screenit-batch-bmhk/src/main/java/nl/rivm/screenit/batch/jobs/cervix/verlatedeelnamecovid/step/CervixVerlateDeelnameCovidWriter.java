package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step;

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
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.LogService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixVerlateDeelnameCovidWriter extends AbstractCervixProjectGroepWriter<CervixBrief>
{
	private final BriefHerdrukkenService briefHerdrukkenService;

	private final BaseProjectService projectService;

	private final LogService logService;

	@Override
	protected void write(CervixBrief brief) throws Exception
	{
		var projectGroep = prepareProjectGroep();
		try
		{
			LOG.info("Voeg client (clientId: " + brief.getClient().getId() + ") toe aan projectgroep '" + projectGroep.getNaam() + "'");
			var projectClient = projectService.addClientToProjectGroep(projectGroep, brief.getClient());
			brief.setAangevraagdeHerdruk(true);
			briefHerdrukkenService.opnieuwAanmaken(brief, null);
			projectClient.setActief(false);
			projectClient.setProjectInactiefDatum(currentDateSupplier.getDate());
		}
		catch (IllegalStateException e)
		{
			var melding = e.getMessage() + ". Geen herdruk van laatste uitnodiging aangemaakt.";
			LOG.warn(melding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CLIENT_GEEN_VERLATE_DEELNAME_UITNODIGING, brief.getClient(), melding, Bevolkingsonderzoek.CERVIX);
		}
	}
}
