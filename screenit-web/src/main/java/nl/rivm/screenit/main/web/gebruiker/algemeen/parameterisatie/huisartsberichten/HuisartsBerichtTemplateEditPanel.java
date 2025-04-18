package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.huisartsberichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.HuisartsBerichtTemplate;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.HuisartsBerichtTemplateRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public class HuisartsBerichtTemplateEditPanel extends GenericPanel<HuisartsBerichtTemplate>
{
	@SpringBean
	private HuisartsBerichtTemplateRepository templateRepository;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final IModel<MergeField> mergeFieldModel;

	public HuisartsBerichtTemplateEditPanel(String id, IModel<HuisartsBerichtTemplate> model)
	{
		super(id, model);

		var actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
			Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
		var level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
		var inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		var form = new ScreenitForm<HuisartsBerichtTemplate>("templateForm", getModel());
		mergeFieldModel = new CompoundPropertyModel<>(new Model<>());

		var mergeFieldContainer = new WebMarkupContainer("mergeFieldContainer");
		mergeFieldContainer.setVisible(!inzien);
		form.add(mergeFieldContainer);

		mergeFieldContainer.add(new ScreenitDropdown<>("mergefield", mergeFieldModel, Arrays.asList(MergeField.values()), new ChoiceRenderer<MergeField>()
		{

			@Override
			public Object getDisplayValue(MergeField object)
			{
				return object.getFieldName();
			}
		}).setNullValid(false));
		mergeFieldContainer.add(new AjaxSubmitLink("invoegen", form)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				if (mergeFieldModel.getObject() != null)
				{
					var template = form.getModelObject();
					var inhoud = template.getBerichtInhoud();
					inhoud += " {" + mergeFieldModel.getObject().getFieldName() + "}";
					template.setBerichtInhoud(inhoud);
				}
			}
		});
		var berichtInhoud = new TextArea<String>("berichtInhoud");
		berichtInhoud.setOutputMarkupId(true);
		berichtInhoud.add(new StringValidator(1, 5000));
		berichtInhoud.setRequired(true);
		berichtInhoud.setEnabled(!inzien);
		form.add(berichtInhoud);
		form.add(new ScreenitIndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var template = form.getModelObject();
				template.setAangepast(currentDateSupplier.getDate());
				templateRepository.save(template);
				logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, ScreenitSession.get().getLoggedInAccount(),
					"Huisartsbericht template: '" + template.getBerichtType().getNaam() + "' aangepast.", Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX);
				info("Template is opgeslagen");
			}

		}.setEnabled(!inzien).setVisible(!inzien && ToegangLevel.LANDELIJK.equals(level)));
		add(form);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(mergeFieldModel);
	}

	protected boolean isMinimumActie(Actie actie, Actie minimaal)
	{
		return Actie.INZIEN.equals(minimaal) && actie == null || actie != null && actie.getNiveau() >= minimaal.getNiveau();
	}
}
