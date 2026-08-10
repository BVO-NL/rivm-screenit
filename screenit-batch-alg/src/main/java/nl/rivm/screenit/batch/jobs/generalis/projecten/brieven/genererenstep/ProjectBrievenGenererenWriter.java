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

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Date;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectMergedBrieven;
import nl.rivm.screenit.repository.algemeen.ProjectBriefActieRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.ProjectUtil;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ProjectBrievenGenererenWriter extends AbstractBrievenGenererenWriter<ProjectBrief, ProjectMergedBrieven>
{

	private final ClientService clientService;

	private final ProjectBriefActieRepository projectBriefActieRepository;

	@Override
	protected ProjectMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{

		var mergedBrieven = new ProjectMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(getScreeningOrganisatie());
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		getHibernateService().saveOrUpdate(mergedBrieven);
		return mergedBrieven;
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return ProjectBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		var brief = (ProjectBrief) context.getBrief();
		ClientBrief<?, ?, ?> orgineleBrief = brief.getBrief();
		if (orgineleBrief != null)
		{
			switch (orgineleBrief.getBevolkingsonderzoek())
			{
			case COLON:
				var colonBrief = (ColonBrief) Hibernate.unproxy(orgineleBrief);
				context.setIntakeAfspraak(colonBrief.getIntakeAfspraak());
				context.setVorigeIntakeAfspraak(colonBrief.getVorigeIntakeAfspraak());
				break;
			case CERVIX:
				var cervixBrief = (CervixBrief) Hibernate.unproxy(orgineleBrief);
				if (cervixBrief.getUitnodiging() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getUitnodiging());
				}
				else if (cervixBrief.getMonster() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getMonster().getUitnodiging());
				}
				else if (cervixBrief.getLabformulier() != null)
				{
					context.setCervixUitnodiging(cervixBrief.getLabformulier().getUitstrijkje().getUitnodiging());
				}
				break;
			}
		}
		if (brief.getProjectClient().getProject().getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.MAMMA))
		{
			context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(context.getClient()));
		}
		context.setProjectBrief(brief);

		var projectClient = brief.getProjectClient();
		var project = projectClient.getProject();
		var projectClientAttributen = projectClient.getAttributen();
		for (var attribuut : project.getProjectAttributen())
		{
			if (!attribuut.getActief())
			{
				continue;
			}
			String value = null;
			for (var papc : projectClientAttributen)
			{
				if (attribuut.equals(papc.getAttribuut()))
				{
					value = papc.getValue();
				}
			}
			context.getProjectAttributen().put(attribuut, value);
		}
	}

	@Override
	public BriefafdrukopdrachtDto maakBriefafdrukopdrachtVoorGegenereerdeBrief(ProjectBrief brief, LocalDateTime timestamp)
	{
		var briefafdrukopdrachtDto = super.maakBriefafdrukopdrachtVoorGegenereerdeBrief(brief, timestamp);
		var actie = projectBriefActieRepository.findById(getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID)).orElseThrow();
		ProjectUtil.verwerktPrintomschrijvingInAfdrukopdracht(actie.getPrintomschrijving(), briefafdrukopdrachtDto);
		return briefafdrukopdrachtDto;
	}

	@Override
	public String getMergedBrievenNaam(ProjectMergedBrieven brieven)
	{
		var actie = getHibernateService().load(ProjectBriefActie.class, getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID));
		var naam = "";
		var sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
		var printomschrijving = actie.getPrintomschrijving();
		if (isOverbruggingssituatieParagonStarted)
		{
			var briefType = brieven.getBriefType();
			naam += briefType != null ? briefType.getBriefCode() : BriefType.FALLBACK_BRIEF_CODE;
			if (StringUtils.isNotBlank(printomschrijving))
			{
				var overruleBriefcode = printomschrijving.contains("_");
				if (overruleBriefcode)
				{

					naam = "";
				}
				naam += printomschrijving.replace(" ", "_");
			}
			naam += "_";
			if (brieven.getCreatieDatum() != null)
			{
				naam += sdf.format(brieven.getCreatieDatum()) + "-";
			}
			if (actie.getProject().getNaam() != null)
			{
				var projectNaam = actie.getProject().getNaam();
				projectNaam = projectNaam.replace(" ", "_");
				naam += projectNaam;
			}
		}
		else
		{
			if (brieven.getCreatieDatum() != null)
			{
				naam += sdf.format(brieven.getCreatieDatum()) + "-";
			}
			if (brieven.getScreeningOrganisatie() != null)
			{
				var soNaam = brieven.getScreeningOrganisatie().getNaam();
				soNaam = soNaam.replaceAll(" ", "_");
				naam += soNaam + "-";
			}
			if (actie.getProject().getNaam() != null)
			{
				var projectNaam = actie.getProject().getNaam();
				projectNaam = projectNaam.replaceAll(" ", "_");
				naam += projectNaam + "-";
			}
			if (StringUtils.isNotBlank(printomschrijving))
			{
				naam += printomschrijving.replace(" ", "_").toLowerCase();
			}
		}
		naam = addPdfCounter(naam);

		return naam + ".pdf";
	}

	@Override
	public IDocument getDocumentDefinitie()
	{
		Long projectBriefActieId = getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		return getHibernateService().load(ProjectBriefActie.class, projectBriefActieId);
	}

	@Override
	public String getTechnischeLoggingMergedBriefAanmaken(ProjectMergedBrieven brieven)
	{
		Long projectBriefActieId = getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		var actie = getHibernateService().load(ProjectBriefActie.class, projectBriefActieId);

		var tekst = "Mergedocument(id = " + brieven.getId() + ") aangemaakt voor ScreeningOrganisatie " + brieven.getScreeningOrganisatie().getNaam();
		if (actie != null)
		{
			if (actie.getProject() != null)
			{
				tekst = tekst + ", project " + actie.getProject().getNaam() + ", projectactie(" + actie.getId() + ") " + actie.getType().name();
			}
		}
		return tekst;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.PROJECT_MERGED_BRIEVEN;
	}

	@Override
	public Long getFileStoreId()
	{
		var stepExecutionContext = getStepExecutionContext();

		var actie = getHibernateService().load(ProjectBriefActie.class, stepExecutionContext.getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID));
		var project = actie.getProject();
		return project.getId();
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.PROJECT_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.PROJECT_ONVOLLEDIG_ADRES;
	}
}
