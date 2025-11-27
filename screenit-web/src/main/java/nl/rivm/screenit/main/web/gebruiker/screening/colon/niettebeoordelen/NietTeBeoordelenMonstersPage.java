package nl.rivm.screenit.main.web.gebruiker.screening.colon.niettebeoordelen;

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

import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ColonZoekFitRegistratieMetBarcodePanel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.MEDEWERKER_SCREENING_NIETTEBEOORDELEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class NietTeBeoordelenMonstersPage extends ColonScreeningBasePage
{

	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private ColonBaseFitService fitService;

	private final IModel<ColonFitRegistratie> ifobtTestModel = new CglibHibernateModel<>();

	private final WebMarkupContainer uitnodigingContainer;

	private final ColonZoekFitRegistratieMetBarcodePanel panel;

	public NietTeBeoordelenMonstersPage()
	{
		panel = new ColonZoekFitRegistratieMetBarcodePanel("scanForIfobttest")
		{

			@Override
			protected void fitRegistratieGevonden(ColonFitRegistratie fitRegistratie, AjaxRequestTarget target)
			{
				super.fitRegistratieGevonden(fitRegistratie, target);
				if (fitRegistratie != null)
				{
					if (fitRegistratie.getUitslag() == null && fitRegistratie.getStatus().magWijzigenNaarStatus(ColonFitRegistratieStatus.NIETTEBEOORDELEN, fitRegistratie)
						&& fitRegistratie.getType() == ColonFitType.GOLD)
					{
						Organisatie ingelogdVoorOrganisatie = ScreenitSession.get().getOrganisatie();
						if (ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.LABORATORIUM))
						{
							fitRegistratie.setFitLaboratorium((ColonFitLaboratorium) ingelogdVoorOrganisatie);
						}
						ifobtTestModel.setObject(fitRegistratie);
						target.add(uitnodigingContainer);
					}
					else
					{
						error(getString("error.is.geen.fit.of.al.beoordeeld"));
					}
				}
				else
				{
					boolean isVerwijderdeBarcode = fitService.isVerwijderdeBarcode(getScanInput());
					error(String.format(getString("error.barcode.niet.gekoppeld"), isVerwijderdeBarcode ? "meer " : ""));
				}

			}

		};
		add(panel);
		uitnodigingContainer = new WebMarkupContainer("uitnodigingContainer", new CompoundPropertyModel<>(ifobtTestModel))
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				setVisible(ModelUtil.nullSafeGet(ifobtTestModel) != null);
				super.onConfigure();
			}

		};
		uitnodigingContainer.setOutputMarkupPlaceholderTag(true);
		add(uitnodigingContainer);

		createStatusForm();
	}

	private void createStatusForm()
	{
		Form<Void> statusForm = new Form<>("statusForm");
		uitnodigingContainer.add(statusForm);

		statusForm.add(new AjaxLink<Void>("opnieuw")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				reset(target);
			}
		});

		statusForm.add(
			new ScreenitDropdown<>("redenNietTeBeoordelen",
				Arrays.stream(RedenNietTeBeoordelen.values())
					.filter(value -> value != RedenNietTeBeoordelen.MANUELE_FOUT && value != RedenNietTeBeoordelen.AFWIJKENDE_MONSTERHOEVEELHEID)
					.toList(),
				new EnumChoiceRenderer<>())
				.setNullValid(true)
				.setRequired(true));

		Organisatie ingelogdVoorOrganisatie = ScreenitSession.get().getOrganisatie();
		statusForm.add(new ScreenitDropdown<>( 
			"fitLaboratorium", 
			new SimpleListHibernateModel<>(organisatieService.getActieveOrganisaties(ColonFitLaboratorium.class)), 
			new ChoiceRenderer<>("naam", "id") 
		) 
			.setNullValid(false).setRequired(true).setEnabled(!ingelogdVoorOrganisatie.getOrganisatieType().equals(OrganisatieType.LABORATORIUM)));

		statusForm.add(new AjaxSubmitLink("opslaan")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				colonDossierService.monsterNietBeoordeelbaar(ModelProxyHelper.deproxy(ifobtTestModel.getObject()));
				logaction(LogGebeurtenis.COLON_FIT_ONBEOORDEELBAAR, ifobtTestModel.getObject());
				reset(target);
				info(getString("message.gegevensopgeslagen"));
			}
		});

		statusForm.add(new AjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				reset(target);
			}
		});
	}

	private void reset(AjaxRequestTarget target)
	{
		panel.reset(target);
		ifobtTestModel.setObject(null);
		target.add(uitnodigingContainer);
	}

	private void logaction(LogGebeurtenis gebeurtenis, ColonFitRegistratie ifobt)
	{
		Account account = ScreenitSession.get().getIngelogdAccount();
		Client client = ifobt.getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(gebeurtenis, account, client, Bevolkingsonderzoek.COLON);
	}
}
