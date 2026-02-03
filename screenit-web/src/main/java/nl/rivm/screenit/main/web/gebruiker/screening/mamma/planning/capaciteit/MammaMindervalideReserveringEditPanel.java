package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

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

import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningMindervalideReserveringDto;
import nl.rivm.screenit.main.exception.MammaMindervalideReserveringException;
import nl.rivm.screenit.main.service.mamma.MammaMindervalideReserveringService;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.component.validator.TijdstipValidator;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;

public class MammaMindervalideReserveringEditPanel extends GenericPanel<PlanningCapaciteitBlokDto>
{
	@SpringBean
	private MammaMindervalideReserveringService mindervalideReserveringService;

	public MammaMindervalideReserveringEditPanel(String id, IModel<PlanningCapaciteitBlokDto> model)
	{
		super(id, model);
		getModelObject().getMindervalideReserveringen().sort(Comparator.comparing(PlanningMindervalideReserveringDto::getVanaf));
		var reserveringenContainer = new WebMarkupContainer("reserveringenContainer");
		reserveringenContainer.setOutputMarkupId(true);

		var mindervalideReserveringen = maakMindervalideReserveringen(reserveringenContainer);
		reserveringenContainer.add(mindervalideReserveringen);
		add(reserveringenContainer);
		voegNieuweReserveringenKnopToe(reserveringenContainer);
	}

	private @NotNull ListView<PlanningMindervalideReserveringDto> maakMindervalideReserveringen(WebMarkupContainer reserveringenContainer)
	{
		var mindervalideReserveringenModel = new PropertyModel<List<PlanningMindervalideReserveringDto>>(getModel(), "mindervalideReserveringen");
		var benodigdeMinuten = mindervalideReserveringService.getBenodigdeMinutenVoorReservering(getModelObject());
		return new ListView<>("mindervalideReserveringen", mindervalideReserveringenModel)
		{
			@Override
			protected void populateItem(ListItem<PlanningMindervalideReserveringDto> item)
			{
				var reserveringDto = item.getModelObject();
				var vanafHH = new TextField<>("vanafUur", new PropertyModel<String>(reserveringDto, "vanafUur"));
				vanafHH.setRequired(true);
				vanafHH.add(TijdstipValidator.uurValidator());

				var vanafMM = new TextField<>("vanafMinuut", new PropertyModel<String>(reserveringDto, "vanafMinuut"));
				vanafMM.setRequired(true);
				vanafMM.add(TijdstipValidator.minuutAfgerondOpVijfValidator());

				var eindTijd = reserveringDto.getVanaf().plusMinutes(benodigdeMinuten);
				item.add(vanafHH, vanafMM, new Label("eind", DateUtil.formatLocalTime(eindTijd)));

				var verwijderButton = new IndicatingAjaxLink<Void>("verwijder")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						var capaciteitBlokDto = MammaMindervalideReserveringEditPanel.this.getModelObject();
						var mindervalideReserveringDtos = capaciteitBlokDto.getMindervalideReserveringen();
						mindervalideReserveringDtos.remove(reserveringDto);
						target.add(reserveringenContainer);
					}
				};
				item.add(verwijderButton);
			}
		};
	}

	private void voegNieuweReserveringenKnopToe(WebMarkupContainer reserveringenContainer)
	{
		var nieuweReserveringButton = new ScreenitIndicatingAjaxButton("nieuweReservering")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					var capaciteitBlokDto = MammaMindervalideReserveringEditPanel.this.getModelObject();
					var reservering = mindervalideReserveringService.maakEerstBeschikbareMindervalideReservering(capaciteitBlokDto);
					var mindervalideReserveringDtos = capaciteitBlokDto.getMindervalideReserveringen();
					mindervalideReserveringDtos.add(reservering);
					mindervalideReserveringDtos.sort(Comparator.comparing(PlanningMindervalideReserveringDto::getVanaf));
					target.add(reserveringenContainer);
				}
				catch (MammaMindervalideReserveringException e)
				{
					error(getString(e.getMessage()));
				}
			}
		};
		add(nieuweReserveringButton);
	}
}
