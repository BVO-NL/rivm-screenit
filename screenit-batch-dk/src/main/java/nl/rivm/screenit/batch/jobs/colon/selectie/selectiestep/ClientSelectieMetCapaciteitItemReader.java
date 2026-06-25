package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Collection;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.batch.service.ColonUitnodigingSelectieService;
import nl.rivm.screenit.batch.service.ColonUitnodigingsgebiedCapaciteitService;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportageGewijzigdGebiedEntry;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseUitnodigingService;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@StepScope
public class ClientSelectieMetCapaciteitItemReader extends AbstractClientSelectieReader
{
	@Autowired
	private ColonUitnodigingSelectieService uitnodigingSelectieService;

	@Autowired
	private ColonUitnodigingsgebiedCapaciteitService uitnodigingsGebiedCapactieitService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private ColonBaseUitnodigingService uitnodigingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private DatabaseRunner databaseRunner;

	private Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden;

	public ClientSelectieMetCapaciteitItemReader()
	{
		setFetchSize(50);
	}

	@Override
	public ClientCategorieEntry read()
	{
		var clientCategorieEntry = getNextEntry();

		if (clientCategorieEntry == null)
		{
			rapporteerLeegLopendeGebieden();
		}
		return clientCategorieEntry;
	}

	@Override
	protected void openInternal(ExecutionContext executionContext) throws ItemStreamException
	{
		var jobParameters = stepExecution.getJobExecution().getJobParameters();
		var herstartJob = Boolean.parseBoolean(jobParameters.getString(JobStartParameter.COLON_SELECTIE_HERSTART.name(), "false"));
		uitnodigingsgebieden = uitnodigingsGebiedCapactieitService.bepaalCapaciteit(executionContext, true, herstartJob);
		setCursor();
	}

	private void setCursor()
	{
		var projectGroepen = projectService.getActieveProjectGroepenVoorUitnodigingDK();

		Integer minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		if (minimaleLeeftijd == null)
		{
			throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
		}

		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}

		var selectieContext = new ColonClientSelectieContext();
		selectieContext.fitService = fitService;
		selectieContext.uitnodigingSelectieService = uitnodigingSelectieService;
		selectieContext.uitnodigingService = uitnodigingService;
		selectieContext.hibernateService = hibernateService;
		selectieContext.uitnodigingsGebiedCapaciteitService = uitnodigingsGebiedCapactieitService;
		selectieContext.fetchSize = fetchSize;
		selectieContext.minimaleLeeftijd = minimaleLeeftijd;
		selectieContext.maximaleLeeftijd = maximaleLeeftijd;
		selectieContext.databaseRunner = databaseRunner;
		selectieContext.peildatum = currentDateSupplier.getLocalDate();
		selectieContext.init(uitnodigingService.getUitnodigingCohorten(), projectGroepen);

		cursor = new ClientSelectieMetCapaciteitItemCursor(selectieContext, uitnodigingsgebieden);
	}

	private void rapporteerLeegLopendeGebieden()
	{
		Long selectieRapportageId = stepExecution.getJobExecution().getExecutionContext().getLong(SelectieConstants.RAPPORTAGEKEYSELECTIE);
		var selectieRapportage = hibernateService.get(ColonSelectieRapportage.class, selectieRapportageId);
		if (selectieRapportage != null)
		{
			for (var uitnodigingsgebied : uitnodigingsgebieden)
			{
				int capaciteitToevoegingOfOver = uitnodigingsgebied.getUitnodigingscapaciteitToevoegingOfOver();
				if (capaciteitToevoegingOfOver > 0 || uitnodigingsgebied.isLeeglopendGebied())
				{
					var gewijzigdGebiedEntry = new ColonSelectieRapportageGewijzigdGebiedEntry();
					gewijzigdGebiedEntry.setPercentage(capaciteitToevoegingOfOver);
					gewijzigdGebiedEntry.setUitnodigingsGebied(hibernateService.load(UitnodigingsGebied.class, uitnodigingsgebied.getUitnodigingsgebiedId()));
					gewijzigdGebiedEntry.setRapportage(selectieRapportage);
					selectieRapportage.getGewijzigdeGebieden().add(gewijzigdGebiedEntry);
					hibernateService.saveOrUpdate(selectieRapportage);
				}
			}
		}
	}

}
