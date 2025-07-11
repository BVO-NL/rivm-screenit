package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TestService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class BijzondereClientDatumPopup extends GenericPanel<List<Client>>
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private TestService testService;

	private final IModel<GbaPersoonDatum> persoonDatumModel;

	public BijzondereClientDatumPopup(String id, IModel<List<Client>> model)
	{
		super(id, model);

		Form<Void> form = new Form<Void>("form");
		add(form);

		List<GbaPersoonDatum> opties = getOpties();
		persoonDatumModel = Model.of(opties.get(0));
		RadioChoice<GbaPersoonDatum> reden = new TestEnumRadioChoice<>("bericht", persoonDatumModel, getOpties(), new NaamChoiceRenderer<>());
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		form.add(reden);

		addButtons(form);

	}

	private List<GbaPersoonDatum> getOpties()
	{
		List<GbaPersoonDatum> opties = new ArrayList<GbaPersoonDatum>();
		Client client = getModelObject().get(0);
		GbaPersoon persoon = client.getPersoon();
		if (persoon.getOverlijdensdatum() == null)
		{
			opties.add(GbaPersoonDatum.DATUM_OVERLIJDEN);
			if (persoon.getDatumVertrokkenUitNederland() == null)
			{
				opties.add(GbaPersoonDatum.DATUM_VERTROKKEN_UIT_NEDERLAND);
			}
			else
			{
				opties.add(GbaPersoonDatum.DATUM_VESTIGING_NEDERLAND);
			}
		}
		return opties;
	}

	private void addButtons(Form form)
	{
		IndicatingAjaxSubmitLink link = new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<Client> clienten = getModelObject();
				for (Client client : clienten)
				{
					var persoon = client.getPersoon();
					var persoonDatum = persoonDatumModel.getObject();
					var adres = persoon.getGbaAdres();
					switch (persoonDatum)
					{
					case DATUM_OVERLIJDEN:
						persoon.setOverlijdensdatum(currentDateSupplier.getDate());
						break;
					case DATUM_VERTROKKEN_UIT_NEDERLAND:
						persoon.setDatumVertrokkenUitNederland(currentDateSupplier.getDate());
						adres.setPostcode(null);
						adres.setHuisnummer(null);
						adres.setStraat(null);
						adres.setPlaats(null);
						adres.setGbaGemeente(testService.getGemeenteByCode(Gemeente.RNI_CODE));
						persoon.setDatumVestigingNederland(null);
						break;
					case DATUM_VESTIGING_NEDERLAND:
						persoon.setDatumVestigingNederland(currentDateSupplier.getDate());
						var gemeente = testService.getEersteGemeenteMetScreeningOrganisatie();
						adres.setPostcode("1234AA");
						adres.setHuisnummer(9);
						adres.setStraat("Teststraat");
						adres.setPlaats(gemeente.getNaam());
						adres.setGbaGemeente(gemeente);
						persoon.setDatumVertrokkenUitNederland(null);
						break;
					}
					clientService.actiesNaUpdateWithGba(client);
					hibernateService.saveOrUpdate(persoon);
				}
				close(target);
			}
		};
		add(link);
	}

	public abstract void close(AjaxRequestTarget target);

	private enum GbaPersoonDatum implements INaam
	{
		DATUM_OVERLIJDEN("Overlijdingsdatum"),

		DATUM_VERTROKKEN_UIT_NEDERLAND("Vertrokken uit Nederland"),

		DATUM_VESTIGING_NEDERLAND("Vestiging in Nederland");

		private final String naam;

		GbaPersoonDatum(String naam)
		{
			this.naam = naam;
		}

		@Override
		public String getNaam()
		{
			return naam;
		}

	}

}
