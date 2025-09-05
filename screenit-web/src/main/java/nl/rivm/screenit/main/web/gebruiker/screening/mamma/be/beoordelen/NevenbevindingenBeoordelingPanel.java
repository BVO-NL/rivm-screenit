package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen;

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

import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaNevenbevindingen;
import nl.rivm.screenit.model.mamma.enums.MammaNevenbevindingenZijde;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public class NevenbevindingenBeoordelingPanel extends GenericPanel<MammaLezing> implements IDetachable
{
	private final IModel<MammaBeoordeling> beoordeling;

	private WebMarkupContainer eersteLezingNevenBevindingenContainer;

	private final WebMarkupContainer nevenbevindingContainer;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	public NevenbevindingenBeoordelingPanel(String id, IModel<MammaLezing> model, IModel<MammaBeoordeling> beoordeling, boolean alleenLezen)
	{
		super(id, model);
		this.beoordeling = beoordeling;

		createNevenbevindingenMultiselect(alleenLezen);
		createEersteLezingResultaat(beoordeling.getObject());

		nevenbevindingContainer = initNevenbevindingContainer();
		add(nevenbevindingContainer);

		var nevenbevindingOpmerking = new TextArea("nevenbevindingOpmerking");
		nevenbevindingOpmerking.add(StringValidator.maximumLength(HibernateMagicNumber.L255));
		nevenbevindingOpmerking.setOutputMarkupId(true);
		nevenbevindingContainer.add(nevenbevindingOpmerking);

		nevenbevindingOpmerking.setEnabled(!alleenLezen);

		var zijdeGroup = getZijdeRadioChoiceGroup();
		zijdeGroup.setRequired(true);

		nevenbevindingContainer.add(zijdeGroup);
	}

	private static RadioChoice<MammaNevenbevindingenZijde> getZijdeRadioChoiceGroup()
	{
		return new RadioChoice<>("nevenbevindingZijde", Arrays.asList(MammaNevenbevindingenZijde.RECHTS, MammaNevenbevindingenZijde.LINKS, MammaNevenbevindingenZijde.BEIDE),
			new EnumChoiceRenderer<>())
		{
			@Override
			protected String getPrefix(int index, MammaNevenbevindingenZijde choice)
			{
				return "<label>";
			}

			@Override
			public String getSuffix()
			{
				return "<span></span></label>";
			}
		};
	}

	private WebMarkupContainer initNevenbevindingContainer()
	{
		var nevenbevindingContainer = new WebMarkupContainer("nevenbevindingContainer");
		nevenbevindingContainer.setOutputMarkupId(true);
		nevenbevindingContainer.setOutputMarkupPlaceholderTag(true);
		nevenbevindingContainer.setVisible(!getModelObject().getNevenbevindingen().isEmpty());
		return nevenbevindingContainer;
	}

	private void createEersteLezingResultaat(MammaBeoordeling beoordeling)
	{
		eersteLezingNevenBevindingenContainer = new WebMarkupContainer("eersteLezingNevenBevindingenContainer");
		eersteLezingNevenBevindingenContainer.setOutputMarkupPlaceholderTag(true);
		eersteLezingNevenBevindingenContainer.setOutputMarkupId(true);
		eersteLezingNevenBevindingenContainer.setVisible(false);
		add(eersteLezingNevenBevindingenContainer);

		if (isTweedeLezingEnHeeftEersteLezingNevenbevinding(beoordeling))
		{
			var eersteLezing = beoordeling.getEersteLezing();
			var nevenbevindingen = baseBeoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, eersteLezing);
			var nevenbevindingenZijdeMetOpmerking = baseBeoordelingService.getNevenbevindingOpmerkingTekst("\n", eersteLezing);

			eersteLezingNevenBevindingenContainer.add(new Label("eersteLezingNevenbevindingen", nevenbevindingen));
			eersteLezingNevenBevindingenContainer.add(new Label("eersteLezingNevenbevindingenOpmerking", nevenbevindingenZijdeMetOpmerking)
				.setVisible(StringUtils.isNotBlank(nevenbevindingenZijdeMetOpmerking)));
		}
	}

	private boolean isTweedeLezingEnHeeftEersteLezingNevenbevinding(MammaBeoordeling beoordeling)
	{
		var eersteLezing = beoordeling.getEersteLezing();
		return (MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus())
			|| MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus()))
			&& eersteLezing != null &&
			!eersteLezing.getNevenbevindingen().isEmpty();
	}

	private void createNevenbevindingenMultiselect(boolean alleenLezen)
	{
		var nevenbevindingenSelector = new ScreenitListMultipleChoice<>("nevenbevindingen",
			alleenLezen ? Arrays.asList(MammaNevenbevindingen.values()) : MammaNevenbevindingen.getActieveNevenbevindingen(), new EnumChoiceRenderer<>());
		nevenbevindingenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				var huidigeLezingHeeftNevenBevindingen = !getModelObject().getNevenbevindingen().isEmpty();

				eersteLezingNevenBevindingenContainer.setVisible(isTweedeLezingEnHeeftEersteLezingNevenbevinding(beoordeling.getObject()) && huidigeLezingHeeftNevenBevindingen);
				nevenbevindingContainer.setVisible(huidigeLezingHeeftNevenBevindingen);

				target.add(nevenbevindingContainer, eersteLezingNevenBevindingenContainer);
			}
		});
		nevenbevindingenSelector.setEnabled(!alleenLezen);
		add(nevenbevindingenSelector);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(this.beoordeling);
	}
}
