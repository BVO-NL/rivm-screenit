package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MammaDoelgroepIndicatorPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.formatter.TelefoonnummersFormatter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.Hibernate;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaAfsprakenBlokPanel extends GenericPanel<List<MammaAfspraakOfMindervalideReserveringWrapper>>
{
	private final HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken;

	public static final String AFSPRAAK_VERZETTEN_DATUM = "AfspraakVerzetten.datum";

	public static final String AFSPRAAK_VERZETTEN_SCREENINGSEENHEID = "AfspraakVerzetten.screeningsEenheid";

	public static final String AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER = "AfspraakVerzetten.komtVanuitAfsprakenKalender";

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private boolean magBulkVerzetten;

	public MammaAfsprakenBlokPanel(String id, IModel<List<MammaAfspraakOfMindervalideReserveringWrapper>> afsprakenEnReserveringenModel,
		HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken,
		LocalDate currentDay,
		boolean magVerzetten, boolean magBulkVerzetten)
	{
		super(id, afsprakenEnReserveringenModel);
		this.selectedAfspraken = selectedAfspraken;
		this.magBulkVerzetten = magBulkVerzetten;

		setOutputMarkupId(true);

		var selectColumn = new WebMarkupContainer("selectColumn");
		selectColumn.setVisible(magBulkVerzetten);
		add(selectColumn);

		add(new ListView<>("items", afsprakenEnReserveringenModel)
		{
			@Override
			protected void populateItem(ListItem<MammaAfspraakOfMindervalideReserveringWrapper> item)
			{
				var wrapper = item.getModelObject();
				if (wrapper.isLegeMindervalideReservering())
				{
					addLabelsVoorMindervalideReservering(item);
					return;
				}

				addLabelsVoorAfspraak(item);
			}

			private void addLabelsVoorAfspraak(ListItem<MammaAfspraakOfMindervalideReserveringWrapper> item)
			{
				var afspraak = item.getModelObject().getAfspraak();
				var dossier = getDossier(afspraak);
				var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
				var client = dossier.getClient();
				var persoon = client.getPersoon();

				addCheckbox(item);
				item.add(DateLabel.forDatePattern("tijd", Model.of(afspraak.getVanaf()), "HH:mm"));

				item.add(new Label("client", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
				item.add(new Label("geboortedatum", DateUtil.getGeboortedatum(persoon)));
				item.add(new Label("bsn", persoon.getBsn()));
				item.add(new Label("telefoonnr", TelefoonnummersFormatter.getTelefoonnummersVoorPersoon(persoon)));
				item.add(new Label("opkomstkans", BigDecimalUtil.decimalToString(afspraak.getOpkomstkans().getOpkomstkans().multiply(BigDecimal.valueOf(100)), 0) + "%"));
				item.add(new MammaDoelgroepIndicatorPanel("doelgroep", dossier, true));
				item.add(new Label("verzet", getVerzetTekst(afspraak)));

				item.add(new WebMarkupContainer("mindervalideReservering").setVisible(item.getModelObject().isMindervalideReserveringOpVanafTijd()));
				var verzetten = createVerzettenLink(item);
				var laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
				verzetten.setVisible(magVerzetten && laatsteAfspraak != null && laatsteAfspraak.equals(afspraak)
					&& !AfmeldingUtil.isEenmaligOfDefinitiefAfgemeld(getDossier(laatsteAfspraak)));
				item.add(verzetten);
			}

			private AjaxLink<Void> createVerzettenLink(ListItem<MammaAfspraakOfMindervalideReserveringWrapper> item)
			{
				return new AjaxLink<>("verzetten")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						var afspraak = (MammaAfspraak) Hibernate.unproxy(ModelProxyHelper.deproxy(item.getModelObject().getAfspraak()));
						List<Object> extraParameters = getExtraParameters(afspraak);

						ScreenitSession.get().setZoekObject(AFSPRAAK_VERZETTEN_DATUM, Model.of(currentDay));
						ScreenitSession.get().setZoekObject(AFSPRAAK_VERZETTEN_SCREENINGSEENHEID, ModelUtil.sModel(afspraak.getStandplaatsPeriode().getScreeningsEenheid()));
						ScreenitSession.get().setZoekObject(AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER, Model.of(true));

						var clientContactActieTypeWrapper = afspraak.getVanaf().compareTo(dateSupplier.getDate()) < 0
							? ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN
							: ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN;

						var client = getDossier(afspraak).getClient();
						setResponsePage(new ClientContactPage(ModelUtil.sModel((Client) Hibernate.unproxy(client)),
							extraParameters, clientContactActieTypeWrapper));
					}

					private List<Object> getExtraParameters(MammaAfspraak afspraak)
					{
						List<Object> extraParameters = new ArrayList<>();
						extraParameters.add(afspraak);
						extraParameters.add(MammaAfspraakStatus.GEPLAND);
						extraParameters.add(getPage().getDefaultModelObject());
						extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
						extraParameters.add(currentDay);
						return extraParameters;
					}
				};
			}
		});
	}

	private static void addLabelsVoorMindervalideReservering(ListItem<MammaAfspraakOfMindervalideReserveringWrapper> item)
	{
		var vanaf = item.getModelObject().getVanafTijd();
		item.add(new Label("tijd", DateUtil.formatLocalTime(vanaf)));
		item.add(new WebMarkupContainer("select").setVisible(false));
		item.add(new WebMarkupContainer("client"));
		item.add(new WebMarkupContainer("geboortedatum"));
		item.add(new WebMarkupContainer("bsn"));
		item.add(new WebMarkupContainer("telefoonnr"));
		item.add(new WebMarkupContainer("opkomstkans"));
		item.add(new WebMarkupContainer("doelgroep"));
		item.add(new WebMarkupContainer("verzet"));
		item.add(new WebMarkupContainer("mindervalideReservering"));
		item.add(new WebMarkupContainer("verzetten").setVisible(false));
	}

	private void addCheckbox(ListItem<MammaAfspraakOfMindervalideReserveringWrapper> item)
	{
		var afspraak = item.getModelObject().getAfspraak();
		var dossier = getDossier(afspraak);
		var persoon = dossier.getClient().getPersoon();

		var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		var laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();

		var select = new CheckBox("select", new PropertyModel<>(selectedAfspraken.getValueMap(), afspraak.getId().toString()));
		select.setVisible(
			magBulkVerzetten
				&& afspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND)
				&& laatsteAfspraak.equals(afspraak)
				&& dossier.getTehuis() == null
				&& !dossier.getDoelgroep().equals(MammaDoelgroep.MINDERVALIDE)
				&& !AfmeldingUtil.isEenmaligOfDefinitiefAfgemeld(dossier)
				&& !MammaScreeningRondeUtil.heeftActiefUitstel(laatsteScreeningRonde)
				&& persoon.getOverlijdensdatum() == null
				&& persoon.getDatumVertrokkenUitNederland() == null
				&& !persoon.getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE)
		);
		item.add(select);
		selectedAfspraken.addObject(afspraak);
	}

	private String getVerzetTekst(MammaAfspraak afspraak)
	{
		if (afspraak.getVerzettenReden() != null)
		{
			return "Ja (" + getString(EnumStringUtil.getPropertyString(afspraak.getVerzettenReden())) + ")";
		}
		return "Nee";
	}

	private MammaDossier getDossier(MammaAfspraak afspraak)
	{
		return afspraak.getUitnodiging().getScreeningRonde().getDossier();
	}
}
