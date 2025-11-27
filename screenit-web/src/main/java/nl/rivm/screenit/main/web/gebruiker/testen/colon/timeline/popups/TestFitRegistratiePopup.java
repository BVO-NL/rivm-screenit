package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.service.colon.ColonStudieRegistratieService;
import nl.rivm.screenit.service.impl.ProjectUitslagenUploadException;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestFitRegistratiePopup extends AbstractTestBasePopupPanel
{
	@SpringBean
	private ColonTestTimelineService testTimelineService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ColonStudieRegistratieService studieRegistratieService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<ColonFitRegistratie> fitRegistratieModel;

	private IModel<ColonFitRegistratie> testModel;

	private WebMarkupContainer fitContainer;

	private final ScreenitDropdown<ColonFitRegistratie> fitRegistratieDropDown;

	private final IModel<Boolean> verlopenModel = Model.of(false);

	private final HashMap<Long, SimpleListHibernateModel<ColonFitRegistratie>> fitRegistratiesMap = new HashMap<>();

	public TestFitRegistratiePopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		fitRegistratieModel = ModelUtil.ccModel(new ColonFitRegistratie());

		fitContainer = getFitContainer();
		fitContainer.setVisible(false);
		add(fitContainer);

		var fitRegistratiesZonderUitslag = new ArrayList<ColonFitRegistratie>();
		for (var ronde : getModelObject().get(0).getColonDossier().getScreeningRondes())
		{
			for (var fitRegistratie : ronde.getFitRegistraties())
			{
				if (heeftGeenAnalyseResultaat(fitRegistratie))
				{
					fitRegistratiesZonderUitslag.add(fitRegistratie);
					var fitRegistraties = new ArrayList<ColonFitRegistratie>();
					fitRegistraties.add(fitRegistratie);
					var fitRegistratiesModel = new SimpleListHibernateModel<>(fitRegistraties);
					fitRegistratiesMap.put(fitRegistratie.getId(), fitRegistratiesModel);
				}
			}
		}
		IModel<List<ColonFitRegistratie>> fitRegistratiesListModel = ModelUtil.listModel(fitRegistratiesZonderUitslag);

		if (getModelObject().size() > 1)
		{
			for (var client : getModelObject().subList(1, getModelObject().size()))
			{
				for (var ronde : client.getColonDossier().getScreeningRondes())
				{
					for (var i = 0; i < ronde.getFitRegistraties().size(); i++)
					{
						var fitRegistratie = ronde.getFitRegistraties().get(i);
						if (heeftGeenAnalyseResultaat(fitRegistratie))
						{
							var testFitRegistraties = fitRegistratiesMap.get(fitRegistratiesZonderUitslag.get(i).getId());
							testFitRegistraties.add(fitRegistratie);
						}
					}
				}
			}
		}
		fitRegistratieDropDown = new ScreenitDropdown<>("uitnodigingen", fitRegistratieModel, fitRegistratiesListModel, new IChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(ColonFitRegistratie fitRegistratie)
			{
				return "FIT(" + fitRegistratie.getBarcode() + ")/Uitnodiging(" + ColonFitRegistratieUtil.getUitnodiging(fitRegistratie).getUitnodigingsId() + ")";
			}

			@Override
			public String getIdValue(ColonFitRegistratie fitRegistratie, int index)
			{
				if (fitRegistratie.getId() != null)
				{
					return fitRegistratie.getId().toString();
				}
				return null;
			}

			@Override
			public ColonFitRegistratie getObject(String id, IModel<? extends List<? extends ColonFitRegistratie>> choices)
			{
				if (id != null)
				{
					return choices.getObject().stream().filter(i -> i.getId().toString().equals(id)).findFirst().orElse(null);
				}
				return null;
			}
		});
		fitRegistratieDropDown.setNullValid(true);
		fitRegistratieDropDown.setRequired(true);
		fitRegistratieDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				var fitRegistratie = fitRegistratieDropDown.getConvertedInput();
				if (fitRegistratie != null)
				{
					testModel = ModelUtil.ccModel(fitRegistratie);
				}
				else
				{
					throw new IllegalStateException("Er moet een FIT aanwezig zijn!");
				}
				var container = getFitContainer();
				fitContainer.replaceWith(container);
				fitContainer = container;
				target.add(fitContainer);

			}

			@Override
			protected boolean getUpdateModel()
			{
				return false;
			}
		});
		add(fitRegistratieDropDown);
	}

	private boolean heeftGeenAnalyseResultaat(ColonFitRegistratie fitRegistratie)
	{
		return fitRegistratie.getUitslag() == null && fitRegistratie.getGeinterpreteerdeUitslag() == null;
	}

	private WebMarkupContainer getFitContainer()
	{
		var container = new WebMarkupContainer("fitContainer");
		container.setOutputMarkupPlaceholderTag(true);

		var studieRegistratie = testModel != null && testModel.getObject().getType() == ColonFitType.STUDIE;
		var uitslagText = new TextField<BigDecimal>("uitslag", new PropertyModel<>(testModel, "uitslag"));
		container.add(uitslagText.setOutputMarkupId(true).setVisible(!studieRegistratie));

		DropDownChoice<ColonGeinterpreteerdeUitslag> geinterpreteerdeUitslagDropDown = new ScreenitDropdown<>("geinterpreteerdeUitslag",
			new PropertyModel<>(testModel, "geinterpreteerdeUitslag"), Arrays.asList(ColonGeinterpreteerdeUitslag.values()), new EnumChoiceRenderer<>());
		container.add(geinterpreteerdeUitslagDropDown.setRequired(true).setVisible(studieRegistratie));

		var uitslagValueModel = Model.of(new BigDecimal(-1));
		var radioGroup = new RadioGroup<BigDecimal>("uitslagSnel", uitslagValueModel);
		radioGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				testModel.getObject().setUitslag(uitslagValueModel.getObject());
				target.add(uitslagText);
			}

		});

		var normWaardeGold = BigDecimal.valueOf(preferenceService.getInteger(PreferenceKey.COLON_FIT_NORM_WAARDE.name())).divide(BigDecimal.valueOf(100));
		radioGroup.setRequired(true);
		add(radioGroup);
		radioGroup.add(new Radio<>("gunstig", Model.of(BigDecimal.ZERO)));
		radioGroup.add(new Radio<>("ongunstig", Model.of(normWaardeGold.add(BigDecimal.ONE))));
		container.add(radioGroup.setVisible(!studieRegistratie));

		container.add(new CheckBox("verlopen", verlopenModel));

		return container;
	}

	@Override
	protected void opslaan()
	{
		List<ColonFitRegistratie> testFitRegistraties = new ArrayList<>();
		if (testModel.getObject().getId() != null)
		{
			var eersteFitRegistratieId = testModel.getObject().getId();
			if (fitRegistratiesMap.containsKey(eersteFitRegistratieId))
			{
				testFitRegistraties = fitRegistratiesMap.get(eersteFitRegistratieId).getObject();
			}
			for (var fitRegistratie : testFitRegistraties)
			{
				if (!ColonFitRegistratieStatus.NIETTEBEOORDELEN.equals(fitRegistratie.getStatus()))
				{
					fitRegistratie.setUitslag(testModel.getObject().getUitslag());
					if (fitRegistratie.getType().equals(ColonFitType.STUDIE))
					{
						try
						{
							studieRegistratieService.controleerUitslagenbestandOpFouten(fitRegistratie, null);
							testTimelineService.fitOntvangen(fitRegistratie.getScreeningRonde().getDossier().getClient(), verlopenModel.getObject(), fitRegistratie, 1);
						}
						catch (ProjectUitslagenUploadException e)
						{
							fitRegistratie.setGeinterpreteerdeUitslag(null);
							hibernateService.saveOrUpdate(fitRegistratie);
							error(e.getMessage());
						}
					}
					else
					{
						checkEnVerwijderGeinterpreteerdeUitslagExtraFitRegistratie(fitRegistratie);
						testTimelineService.fitOntvangen(fitRegistratie.getScreeningRonde().getDossier().getClient(), verlopenModel.getObject(), fitRegistratie, 1);
					}
				}
			}
		}
	}

	private void checkEnVerwijderGeinterpreteerdeUitslagExtraFitRegistratie(ColonFitRegistratie fitRegistratie)
	{
		var uitnodiging = fitRegistratie.getUitnodiging();
		if (uitnodiging != null && uitnodiging.getGekoppeldeExtraFitRegistratie() != null)
		{
			var andereFit = uitnodiging.getGekoppeldeExtraFitRegistratie();
			if (ColonFitType.STUDIE == andereFit.getType() && andereFit.getVerwerkingsDatum() == null && andereFit.getGeinterpreteerdeUitslag() != null)
			{
				andereFit.setGeinterpreteerdeUitslag(null);
			}
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		for (IModel<List<ColonFitRegistratie>> fitRegistraties : fitRegistratiesMap.values())
		{
			ModelUtil.nullSafeDetach(fitRegistraties);
		}
		ModelUtil.nullSafeDetach(fitRegistratieModel);
		ModelUtil.nullSafeDetach(testModel);
	}
}
