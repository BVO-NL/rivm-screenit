package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaCeWerklijstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.OrganisatieService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaCeZoekPanel extends GenericPanel<MammaCeWerklijstZoekObject>
{
	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private MammaCeWerklijstService ceWerklijstService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	private final AbstractMammaCeWerklijst werklijst;

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenhedenSelector;

	private final WebMarkupContainer resultTable;

	private final Form<MammaCeWerklijstZoekObject> zoekForm;

	private ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingseenhedenSelector;

	private boolean toonSeFilter;

	protected MammaCeZoekPanel(String id, IModel<MammaCeWerklijstZoekObject> model, AbstractMammaCeWerklijst werklijst, WebMarkupContainer resultTable)
	{
		super(id, model);
		this.werklijst = werklijst;
		this.resultTable = resultTable;
		zoekForm = new Form<>("form", getModel());
		createZoekForm();
		zetZoekobjectModel();
	}

	private void createZoekForm()
	{
		var teTonenCeFilters = getTeTonenCeFilters();
		toonSeFilter = teTonenCeFilters.contains(MammaCeFilter.SE);
		var mogelijkeCentraleEenheden = organisatieService.getMogelijkeCentraleEenheden(ScreenitSession.get().getOrganisatie());
		var mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden(mogelijkeCentraleEenheden);

		var centraleEenhedenContainer = new WebMarkupContainer("centraleEenhedenContainer");
		centraleEenhedenContainer.add(createCentraleEenhedenSelector(mogelijkeCentraleEenheden));
		centraleEenhedenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.CE));
		centraleEenhedenContainer.setOutputMarkupId(true);

		var beoordelingsEenhedenContainer = new WebMarkupContainer("beoordelingsEenhedenContainer");
		beoordelingseenhedenSelector = createBeoordelingseenhedenSelector(mogelijkeBeoordelingsEenheden);
		beoordelingsEenhedenContainer.add(beoordelingseenhedenSelector);
		beoordelingsEenhedenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.BE));
		beoordelingsEenhedenContainer.setOutputMarkupId(true);

		var screeningsEenhedenContainer = new WebMarkupContainer("screeningsEenhedenContainer");
		screeningsEenhedenSelector = createScreeningsEenhedenSelector(toonSeFilter ? mogelijkeBeoordelingsEenheden : new ArrayList<>());
		screeningsEenhedenContainer.add(screeningsEenhedenSelector);
		screeningsEenhedenContainer.setVisible(toonSeFilter);
		screeningsEenhedenContainer.setOutputMarkupId(true);

		var beoordelingStatussenContainer = new WebMarkupContainer("beoordelingStatussenContainer");
		var onderzoekStatusSelector = new ScreenitListMultipleChoice<MammaBeoordelingStatus>("beoordelingStatussen",
			getMammaMogelijkeBeoordelingFilterStatussen(), new EnumChoiceRenderer<>(this));
		beoordelingStatussenContainer.add(onderzoekStatusSelector);
		beoordelingStatussenContainer.setVisible(teTonenCeFilters.contains(MammaCeFilter.STATUS));
		beoordelingStatussenContainer.setOutputMarkupId(true);

		zoekForm.add(centraleEenhedenContainer);
		zoekForm.add(beoordelingsEenhedenContainer);
		zoekForm.add(screeningsEenhedenContainer);
		zoekForm.add(beoordelingStatussenContainer);
		addZoekButton();
		add(zoekForm);
	}

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> createScreeningsEenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		var mogelijkeScreeningsEenheden = getMogelijkeScreeningsEenheden(mogelijkeBeoordelingsEenheden);
		return new ScreenitListMultipleChoice<>("screeningsEenheden", ModelUtil.listRModel(mogelijkeScreeningsEenheden), new ChoiceRenderer<>("naam"));
	}

	private void addZoekButton()
	{
		var zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				zetZoekobjectModel();
				target.add(resultTable, zoekForm);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	private List<MammaScreeningsEenheid> getMogelijkeScreeningsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		return toonSeFilter
			? ceWerklijstService.zoekScreeningsEenhedenMetCeWerklijstBeoordeling(getMammaMogelijkeBeoordelingFilterStatussen(), beoordelingsEenheden)
			: new ArrayList<>();
	}

	private void zetZoekobjectModel()
	{
		var zoekObject = getModelObject();
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingStatussen()))
		{
			zoekObject.setBeoordelingStatussen(getMammaMogelijkeBeoordelingFilterStatussen());
			zoekObject.getBeoordelingStatussen().removeAll(getRemoveFromDefaultFilter());
		}
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingsEenheden()))
		{
			zoekObject.setBeoordelingsEenheden(getMogelijkeBeoordelingsEenheden());
		}
		if (CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
		{
			resetSeKeuzelijst();
		}
		ScreenitSession.get().setZoekObject(werklijst.getPageClass(), zoekForm.getModel());
	}

	protected abstract List<MammaBeoordelingStatus> getRemoveFromDefaultFilter();

	protected abstract List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen();

	protected List<MammaCeFilter> getTeTonenCeFilters()
	{
		return Arrays.asList(MammaCeFilter.CE, MammaCeFilter.BE, MammaCeFilter.SE, MammaCeFilter.STATUS);
	}

	private ScreenitListMultipleChoice<CentraleEenheid> createCentraleEenhedenSelector(List<CentraleEenheid> mogelijkeCentraleEenheden)
	{
		var centraleEenhedenSelector = new ScreenitListMultipleChoice<CentraleEenheid>("centraleEenheden",
			ModelUtil.listRModel(mogelijkeCentraleEenheden), new ChoiceRenderer<>("naam"));
		centraleEenhedenSelector.setRequired(true);

		centraleEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetBeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return centraleEenhedenSelector;
	}

	private void resetBeKeuzelijst()
	{
		var zoekObject = getModelObject();
		var gekozenCEs = zoekObject.getCentraleEenheden();
		var mogelijkeBEs = getMogelijkeBeoordelingsEenheden(gekozenCEs);
		beoordelingseenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeBEs));
		zoekObject.setBeoordelingsEenheden(mogelijkeBEs);
		resetSeKeuzelijst();
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden(List<CentraleEenheid> gekozenCEs)
	{
		return gekozenCEs == null || gekozenCEs.isEmpty() ? beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie())
			: beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie(), gekozenCEs);
	}

	private ScreenitListMultipleChoice<BeoordelingsEenheid> createBeoordelingseenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		var beoordelingsEenhedenSelector = new ScreenitListMultipleChoice<BeoordelingsEenheid>("beoordelingsEenheden",
			ModelUtil.listRModel(mogelijkeBeoordelingsEenheden), new ChoiceRenderer<>("naam"));
		beoordelingsEenhedenSelector.setRequired(true);
		beoordelingsEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetSeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return beoordelingsEenhedenSelector;
	}

	private void resetSeKeuzelijst()
	{
		var zoekObject = getModelObject();
		var gekozenBEs = zoekObject.getBeoordelingsEenheden();
		var mogelijkeSEs = getMogelijkeScreeningsEenheden(gekozenBEs);
		screeningsEenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeSEs));
		zoekObject.setScreeningsEenheden(mogelijkeSEs);
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden()
	{
		return beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie());
	}
}
