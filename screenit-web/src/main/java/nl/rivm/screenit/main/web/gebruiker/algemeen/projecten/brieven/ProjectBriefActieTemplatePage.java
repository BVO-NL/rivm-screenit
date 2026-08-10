package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerBasePage;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.AjaxEditableLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.aspose.words.Document;

@Slf4j
public class ProjectBriefActieTemplatePage extends ProjectTemplateTestenBasePage
{
	private final IModel<ProjectBriefActie> briefactieModel;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private HibernateService hibernateService;

	public ProjectBriefActieTemplatePage(IModel<Project> model, IModel<ProjectBriefActie> briefactieModel)
	{
		super(model);
		this.briefactieModel = briefactieModel;

		maakTerugKnop();
	}

	@Override
	protected List<ScreeningOrganisatie> getRegios()
	{
		List<ScreeningOrganisatie> soLijst = new ArrayList<>();
		var project = briefactieModel.getObject().getProject();
		if (project != null && project.getScreeningOrganisaties() != null)
		{
			for (var organisatie : project.getScreeningOrganisaties())
			{
				soLijst.add(hibernateService.load(ScreeningOrganisatie.class, organisatie.getId()));
			}
		}
		return soLijst;
	}

	private void maakTerugKnop()
	{
		add(new Link<Void>("terugViaTitle")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ProjectBriefActiePage(getProjectModel()));
			}

		});
	}

	private String getLaatstGewijzigdDatum()
	{
		var projectBriefActie = briefactieModel.getObject();
		var laatstGewijzigd = "";
		var datum = projectBriefActie.getLaatstGewijzigd();
		if (datum != null)
		{
			laatstGewijzigd = DateUtil.formatShortDate(datum);
		}
		return laatstGewijzigd;
	}

	private String getSoortText()
	{
		var projectBriefActie = briefactieModel.getObject();
		var soortText = "";
		if (projectBriefActie != null && projectBriefActie.getType() != null)
		{
			soortText = getString(EnumStringUtil.getPropertyString(projectBriefActie.getType()));
		}
		return soortText;
	}

	private String getMomentText()
	{
		var projectBriefActie = briefactieModel.getObject();
		var momentTekst = "";
		var type = projectBriefActie.getType();

		if (ProjectBriefActieType.DATUM.equals(type))
		{
			momentTekst = DateUtil.formatShortDate(projectBriefActie.getDatum());
		}
		else if (ProjectBriefActieType.XDAGENNAY.equals(type))
		{
			momentTekst = projectBriefActie.getAantalDagen() + " dagen na ";
			var briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		else if (ProjectBriefActieType.XMETY.equals(type))
		{
			momentTekst = "Tegelijk met ";
			var briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		else if (ProjectBriefActieType.VERVANGENDEBRIEF.equals(type))
		{
			var briefType = projectBriefActie.getBriefType();
			if (briefType != null)
			{
				momentTekst += briefType.getWeergaveNaam();
			}
		}
		return momentTekst;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(briefactieModel);
	}

	@Override
	protected Class<? extends MedewerkerBasePage> getActiveContextMenuClass()
	{
		return ProjectBriefActiePage.class;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		form.add(new Label("naam", new PropertyModel<>(briefactieModel, "document.naam")));
		form.add(new Label("soort", Model.of(getSoortText())));
		form.add(new Label("moment", Model.of(getMomentText())));
		form.add(new Label("laatstGewijzigd", Model.of(getLaatstGewijzigdDatum())));
		form.add(new AjaxEditableLabel<String>("printomschrijving", new PropertyModel<>(briefactieModel, ProjectBriefActie_.PRINTOMSCHRIJVING))
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				hibernateService.saveOrUpdate(briefactieModel.getObject());
			}
		});
	}

	@Override
	protected List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return briefactieModel.getObject().getProject().getBevolkingsonderzoeken();
	}

	@Override
	protected BriefType getParagonBriefType()
	{
		return briefactieModel.getObject().getBriefType();
	}

	@Override
	protected BriefafdrukopdrachtDto maakBriefafdrukopdrachtDto(BriefType briefType, String bestandsNaam)
	{
		var briefafdrukopdrachtDto = super.maakBriefafdrukopdrachtDto(briefType, bestandsNaam);
		ProjectUtil.verwerktPrintomschrijvingInAfdrukopdracht(briefactieModel.getObject().getPrintomschrijving(), briefafdrukopdrachtDto);
		return briefafdrukopdrachtDto;
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		return asposeService.processDocument(briefTemplate, context);
	}

	@Override
	protected File getBriefTemplateFile()
	{
		return uploadDocumentService.load(briefactieModel.getObject().getDocument());
	}

}
