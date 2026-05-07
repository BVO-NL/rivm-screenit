package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;
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

public abstract class MammaFotobesprekingZoekPanel extends GenericPanel<MammaFotobesprekingWerklijstZoekObject>
{
	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	private ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingseenhedenSelector;

	private final Form<MammaFotobesprekingWerklijstZoekObject> zoekForm;

	public MammaFotobesprekingZoekPanel(String id, IModel<MammaFotobesprekingWerklijstZoekObject> model)
	{
		super(id, model);
		zoekForm = new Form<>("form", getModel());
		add(zoekForm);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		vulZoekFormulier();
		zetZoekobjectModel();
	}

	private void vulZoekFormulier()
	{
		var mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden();

		var centraleEenhedenContainer = new WebMarkupContainer("centraleEenhedenContainer");
		centraleEenhedenContainer.add(maakCentraleEenhedenSelector());
		centraleEenhedenContainer.setOutputMarkupId(true);
		centraleEenhedenContainer.setVisible(isIngelogdAlsLandelijk());

		var beoordelingsEenhedenContainer = new WebMarkupContainer("beoordelingsEenhedenContainer");
		beoordelingseenhedenSelector = maakBeoordelingseenhedenSelector(mogelijkeBeoordelingsEenheden);
		beoordelingsEenhedenContainer.add(beoordelingseenhedenSelector);
		beoordelingsEenhedenContainer.setOutputMarkupId(true);

		var typeContainer = new WebMarkupContainer("typeContainer");
		var typeSelector = new ScreenitListMultipleChoice<>("types", List.of(MammaFotobesprekingType.values()), new EnumChoiceRenderer<>(this));
		typeContainer.add(typeSelector);
		typeContainer.setOutputMarkupId(true);

		zoekForm.add(centraleEenhedenContainer);
		zoekForm.add(beoordelingsEenhedenContainer);
		zoekForm.add(typeContainer);
		voegZoekKnopToe();
	}

	private boolean isIngelogdAlsLandelijk()
	{
		return ScreenitSession.get().getOrganisatie().getOrganisatieType() == OrganisatieType.RIVM;
	}

	private void voegZoekKnopToe()
	{
		var zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				zetZoekobjectModel();
				MammaFotobesprekingZoekPanel.this.onZoeken(target, MammaFotobesprekingZoekPanel.this.getModel());
				target.add(zoekForm);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	protected abstract void onZoeken(AjaxRequestTarget target, IModel<MammaFotobesprekingWerklijstZoekObject> zoekModel);

	private void zetZoekobjectModel()
	{
		var zoekObject = getModelObject();
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingsEenheden()))
		{
			zoekObject.setBeoordelingsEenheden(getMogelijkeBeoordelingsEenheden(zoekObject.getCentraleEenheden()));
		}
		ScreenitSession.get().setZoekObject(getPage().getPageClass(), zoekForm.getModel());
	}

	private ScreenitListMultipleChoice<CentraleEenheid> maakCentraleEenhedenSelector()
	{
		var mogelijkeCentraleEenheden = getMogelijkeCentraleEenheden();
		var centraleEenhedenSelector = new ScreenitListMultipleChoice<>("centraleEenheden", ModelUtil.listRModel(mogelijkeCentraleEenheden),
			new ChoiceRenderer<>("naam"));

		centraleEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				herstelBeoordelingseenhedenKeuzelijst();
				target.add(zoekForm);
			}
		});

		return centraleEenhedenSelector;
	}

	private void herstelBeoordelingseenhedenKeuzelijst()
	{
		var zoekObject = getModelObject();
		var gekozenCentraleEenheden = zoekObject.getCentraleEenheden();
		var mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden(gekozenCentraleEenheden);
		beoordelingseenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeBeoordelingsEenheden));
		zoekObject.setBeoordelingsEenheden(mogelijkeBeoordelingsEenheden);
	}

	private ScreenitListMultipleChoice<BeoordelingsEenheid> maakBeoordelingseenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		var beoordelingsEenhedenSelector = new ScreenitListMultipleChoice<>("beoordelingsEenheden", ModelUtil.listRModel(mogelijkeBeoordelingsEenheden),
			new ChoiceRenderer<>("naam"));

		beoordelingsEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(zoekForm);
			}
		});

		return beoordelingsEenhedenSelector;
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden()
	{
		return beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie());
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden(List<CentraleEenheid> centraleEenheden)
	{
		return CollectionUtils.isEmpty(centraleEenheden) ? beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie())
			: beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getOrganisatie(), centraleEenheden);
	}

	private List<CentraleEenheid> getMogelijkeCentraleEenheden()
	{
		return organisatieService.getMogelijkeCentraleEenheden(ScreenitSession.get().getOrganisatie());
	}
}
