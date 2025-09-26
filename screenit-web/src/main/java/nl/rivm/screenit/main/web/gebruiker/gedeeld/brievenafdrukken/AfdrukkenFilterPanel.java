package nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken;

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

import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectMergedBrieven;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public abstract class AfdrukkenFilterPanel<MB extends MergedBrieven<?>> extends GenericPanel<MergedBrievenFilter<MB>>
{
	public AfdrukkenFilterPanel(String id, IModel<MergedBrievenFilter<MB>> model, List<BriefType> briefTypes)
	{
		super(id, model);
		var form = new Form<>("form", new CompoundPropertyModel<>(model));
		add(form);

		var afdrukkenBtn = new AjaxLink<>("afdrukken")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				model.getObject().setGeprint(false);
				model.getObject().setControle(null);
				AfdrukkenFilterPanel.this.doFilter(model, target);
			}
		};
		form.add(afdrukkenBtn);

		var controlerenBtn = new AjaxLink<>("controleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				model.getObject().setGeprint(true);
				model.getObject().setControle(false);
				AfdrukkenFilterPanel.this.doFilter(model, target);
			}
		};
		form.add(controlerenBtn);

		var projectbrievenFilter = model.getObject().getMergedBrievenClass() == ProjectMergedBrieven.class;
		form.add(new TextField<>("naam", new PropertyModel<>(model, "naam")).setVisible(projectbrievenFilter));

		var briefTypeSelect = new ScreenitListMultipleChoice<>("briefTypes", new PropertyModel<>(model, "briefTypes"), briefTypes, new ChoiceRenderer<>("codeEnNaam"))
			.setOutputMarkupId(true)
			.setVisible(!projectbrievenFilter);
		form.add(briefTypeSelect);

		var datumVanaf = ComponentHelper.newDatePicker("vanaf");
		form.add(datumVanaf);
		var datumTot = ComponentHelper.newDatePicker("tot");
		form.add(datumTot);
		form.add(new DependantDateValidator(datumVanaf, datumTot, DependantDateValidator.Operator.AFTER));

		form.add(new IndicatingAjaxSubmitLink("filteren", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				var filterModel = AfdrukkenFilterPanel.this.getModel();
				AfdrukkenFilterPanel.this.doFilter(filterModel, target);
			}
		});
	}

	protected abstract void doFilter(IModel<MergedBrievenFilter<MB>> filterModel, AjaxRequestTarget target);
}
