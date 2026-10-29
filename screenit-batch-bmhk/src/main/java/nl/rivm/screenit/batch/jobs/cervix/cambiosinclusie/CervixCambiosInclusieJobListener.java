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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.GroepSelectieType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.repository.algemeen.ProjectGroepRepository;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.OrganisatieService;

import org.apache.commons.lang3.StringUtils;
import org.jspecify.annotations.NonNull;
import org.springframework.batch.core.job.JobExecution;
import org.springframework.batch.core.step.StepExecution;
import org.springframework.batch.infrastructure.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@RequiredArgsConstructor
public class CervixCambiosInclusieJobListener extends BaseLogListener
{
	private static final String CAMBIOS_INACTIVEREN_STEP_NAAM = "cambiosInactiverenStep";

	private final OrganisatieParameterService organisatieParameterService;

	private final OrganisatieService organisatieService;

	private final ProjectGroepRepository projectGroepRepository;

	private final BaseProjectService projectService;

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);
		var context = jobExecution.getExecutionContext();

		fillProjectIdInJobContext(OrganisatieParameterKey.CERVIX_PROJECT_CAMBIOS_UITSTRIJKJE, context, CervixCambiosInclusieConstants.PROJECT_ID_UITSTRIJKJE_ACTIEF,
			CervixCambiosInclusieConstants.PROJECT_ID_UITSTRIJKJE);

		fillProjectIdInJobContext(OrganisatieParameterKey.CERVIX_PROJECT_CAMBIOS_ZAS, context, CervixCambiosInclusieConstants.PROJECT_ID_ZAS_ACTIEF,
			CervixCambiosInclusieConstants.PROJECT_ID_ZAS);

		var laboratoriumIds = organisatieService.getActieveOrganisaties(BMHKLaboratorium.class).stream()
			.filter(laboratorium -> Boolean.TRUE.equals(
				organisatieParameterService.getOrganisatieParameter(laboratorium, OrganisatieParameterKey.CERVIX_CAMBIOS_LABORATORIUM, Boolean.FALSE)))
			.map(BMHKLaboratorium::getId)
			.toList();
		if (!laboratoriumIds.isEmpty())
		{
			context.putString(CervixCambiosInclusieConstants.LABORATORIUM_IDS, laboratoriumIds.stream().map(String::valueOf).collect(Collectors.joining(",")));
		}
	}

	private void fillProjectIdInJobContext(OrganisatieParameterKey organisatieParameterKey, ExecutionContext context, String projectIdActiefParameterKey,
		String projectIdParameterKey)
	{
		String projectNaam = organisatieParameterService.getOrganisatieParameter(null, organisatieParameterKey);
		if (StringUtils.isNotBlank(projectNaam))
		{
			var zoekObject = new Project();

			zoekObject.setNaam(projectNaam);
			zoekObject.setGroepSelectieType(GroepSelectieType.DYNAMISCH);
			zoekObject.getProjectTypes().add(ProjectType.PROJECT);
			zoekObject.getBevolkingsonderzoeken().add(Bevolkingsonderzoek.CERVIX);
			zoekProjectEnZetIdInContext(context, projectIdParameterKey, zoekObject);

			zoekObject.getProjectStatussen().add(ProjectStatus.ACTIEF);
			zoekProjectEnZetIdInContext(context, projectIdActiefParameterKey, zoekObject);
		}
	}

	private void zoekProjectEnZetIdInContext(ExecutionContext context, String projectIdParameterKey, Project zoekObject)
	{
		var projecten = projectService.getProjecten(zoekObject, Collections.emptyList(), Collections.emptyList(), -1, -1, new SortState<>(Project_.NAAM, true));
		Project project = null;
		if (!projecten.isEmpty())
		{
			project = projecten.getFirst();
		}
		if (project != null)
		{
			context.putLong(projectIdParameterKey, project.getId());
		}
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_CAMBIOS_INCLUSIE_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_CAMBIOS_INCLUSIE_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var event = super.eindLogging(jobExecution);
		if (event.getLevel() == Level.INFO)
		{
			var context = jobExecution.getExecutionContext();
			var meldingen = new ArrayList<String>();
			var niveau = rapporteerInclusieJob(context, meldingen);

			niveau = niveau.geefHoogsteLevelVanTweeLevels(
				rapporteerInclusieStap(context, meldingen,
					CervixCambiosInclusieConstants.PROJECT_ID_UITSTRIJKJE_ACTIEF,
					CervixCambiosInclusieConstants.PROJECT_GROEP_ID_UITSTRIJKJE,
					OrganisatieParameterKey.CERVIX_PROJECT_CAMBIOS_UITSTRIJKJE));
			niveau = niveau.geefHoogsteLevelVanTweeLevels(
				rapporteerInclusieStap(context, meldingen,
					CervixCambiosInclusieConstants.PROJECT_ID_ZAS_ACTIEF,
					CervixCambiosInclusieConstants.PROJECT_GROEP_ID_ZAS,
					OrganisatieParameterKey.CERVIX_PROJECT_CAMBIOS_ZAS));

			rapporteerInactiveerStap(jobExecution, meldingen);

			event.setLevel(niveau);
			event.setMelding(String.join(" ", meldingen));
		}
		return event;
	}

	private static @NonNull Level rapporteerInclusieJob(ExecutionContext context, List<String> meldingen)
	{
		var heeftGeenProjecten =
			!context.containsKey(CervixCambiosInclusieConstants.PROJECT_ID_UITSTRIJKJE_ACTIEF) && !context.containsKey(CervixCambiosInclusieConstants.PROJECT_ID_ZAS_ACTIEF);
		var heeftGeenLaboratorium = !context.containsKey(CervixCambiosInclusieConstants.LABORATORIUM_IDS);
		if (heeftGeenLaboratorium)
		{
			meldingen.add("Geen enkel BMHK-laboratorium is aangevinkt voor de Cambiosstudie.");
		}
		return heeftGeenProjecten || heeftGeenLaboratorium ? Level.ERROR : Level.INFO;
	}

	private Level rapporteerInclusieStap(ExecutionContext context, List<String> meldingen, String projectIdKey, String projectGroepIdKey,
		OrganisatieParameterKey organisatieParameterKey)
	{
		String projectNaam = organisatieParameterService.getOrganisatieParameter(null, organisatieParameterKey);
		if (!context.containsKey(projectIdKey))
		{
			meldingen.add("Project '" + projectNaam + "' niet bekend, niet (meer) actief of niet de juiste configuratie.");
			return Level.INFO;
		}
		if (context.containsKey(projectGroepIdKey))
		{
			var groepId = context.getLong(projectGroepIdKey);
			var groep = projectGroepRepository.findById(groepId).orElse(null);
			if (groep != null)
			{
				if (groep.getPopulatie() > 0)
				{
					meldingen.add(groep.getPopulatie() + " client(en) toegevoegd aan project '" + projectNaam + "' in groep '" + groep.getNaam() + "'.");
					return Level.INFO;
				}
				LOG.error("0 populatie in groep met id '{}' mag niet voorkomen", groepId);
			}
			else
			{
				LOG.error("geen entiteit voor groep met id '{}' gevonden", groepId);
			}
			meldingen.add("Fout bij bij toevoegen van client(en) aan project '" + projectNaam + "'.");
			return Level.ERROR;
		}
		else
		{
			meldingen.add("Geen clienten gevonden voor project '" + projectNaam + "'.");
		}
		return Level.INFO;
	}

	private static void rapporteerInactiveerStap(JobExecution jobExecution, List<String> meldingen)
	{
		var geinactiveerd = jobExecution.getStepExecutions().stream()
			.filter(stepExecution -> CAMBIOS_INACTIVEREN_STEP_NAAM.equals(stepExecution.getStepName()))
			.mapToLong(StepExecution::getWriteCount)
			.sum();
		if (geinactiveerd > 0)
		{
			meldingen.add(geinactiveerd + " client(en) geïnactiveerd na ontvangst van de vervangende brief.");
		}
	}
}
