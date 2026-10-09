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

import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.AbstractCervixProjectGroepWriter;
import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.repository.algemeen.ProjectGroepRepository;
import nl.rivm.screenit.repository.algemeen.ProjectRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.OrganisatieService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.job.JobExecution;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftNaam;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftType;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.isActiefOpDatum;

@Component
@Slf4j
@RequiredArgsConstructor
public class CervixCambiosInclusieJobListener extends BaseLogListener
{
	private final OrganisatieParameterService organisatieParameterService;

	private final OrganisatieService organisatieService;

	private final ProjectRepository projectRepository;

	private final ProjectGroepRepository projectGroepRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private String projectNaam;

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

		projectNaam = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_PROJECT_CAMBIOS);
		if (StringUtils.isNotBlank(projectNaam))
		{
			projectRepository.findFirst(
					heeftNaam(projectNaam).and(heeftType(ProjectType.PROJECT)).and(isActiefOpDatum(currentDateSupplier.getLocalDate())),
					Sort.by(Sort.Order.asc(Project_.NAAM)))
				.ifPresent(project -> context.putLong(AbstractCervixProjectGroepWriter.PROJECT_ID, project.getId()));
		}

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
			String melding;
			var context = jobExecution.getExecutionContext();
			var heeftProject = context.containsKey(AbstractCervixProjectGroepWriter.PROJECT_ID);
			var heeftLaboratorium = context.containsKey(CervixCambiosInclusieConstants.LABORATORIUM_IDS);
			if (heeftProject && heeftLaboratorium)
			{
				melding = "Geen clienten gevonden voor project '" + projectNaam + "'";
				event.setLevel(Level.WARNING);
				if (context.containsKey(AbstractCervixProjectGroepWriter.PROJECT_GROEP_ID))
				{
					var groepId = context.getLong(AbstractCervixProjectGroepWriter.PROJECT_GROEP_ID);
					var groep = projectGroepRepository.findById(groepId).orElse(null);
					if (groep != null)
					{
						if (groep.getPopulatie() > 0)
						{
							melding = groep.getPopulatie() + " client(en) toegevoegd aan project '" + projectNaam + "' in groep '" + groep.getNaam() + "'.";
							event.setLevel(Level.INFO);
						}
					}
					else
					{
						LOG.error("geen entiteit voor groep met id " + groepId + " gevonden");
					}
				}
			}
			else if (!heeftProject)
			{
				melding = "Project '" + StringUtils.defaultIfEmpty(projectNaam, "<leeg>") + "' is niet bekend, niet actief of niet van het type PROJECT.";
				event.setLevel(Level.ERROR);
			}
			else
			{
				melding = "Geen enkel BMHK-laboratorium is aangevinkt voor de Cambiosstudie.";
				event.setLevel(Level.ERROR);
			}
			event.setMelding(melding);
		}
		return event;
	}
}
