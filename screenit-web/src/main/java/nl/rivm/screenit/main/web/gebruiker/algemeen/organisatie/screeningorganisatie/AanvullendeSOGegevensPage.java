package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.screeningorganisatie;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.PercentageIntegerField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.pingpong.PingPongInput;
import nl.rivm.screenit.main.web.component.validator.ScreenITIBANValidator;
import nl.rivm.screenit.main.web.component.validator.ScreenitUniqueFieldValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadOrganisatieImageFormComponent;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadOrganisatieImageType;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.GemeenteService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = {
		Recht.MEDEWERKER_SCREENINGS_ORG_BEHEER },
	checkScope = true,
	level = ToegangLevel.ORGANISATIE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON,
		Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class AanvullendeSOGegevensPage extends OrganisatieBeheer
{
	@SpringBean
	private GemeenteService gemeenteService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	public AanvullendeSOGegevensPage()
	{
		var organisatie = getCurrentSelectedOrganisatie();
		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(organisatie)));

		var model = ModelUtil.cModel(organisatie);
		setDefaultModel(model);
		organisatie = model.getObject();
		if (organisatie.getAdres() == null)
		{
			organisatie.setAdres(new Adres());
		}
		if (organisatie.getPostbusAdres() == null)
		{
			organisatie.setPostbusAdres(new Adres());
		}
		if (organisatie.getAntwoordnummerAdres() == null)
		{
			organisatie.setAntwoordnummerAdres(new Adres());
		}

		var form = new ScreenitForm<Organisatie>("form", model);
		add(form);

		var organisatieType = organisatie.getOrganisatieType();
		var recht = organisatieType.getRecht();

		var actie = autorisatieService.getActieVoorOrganisatie(getIngelogdeOrganisatieMedewerker(), organisatie, recht);

		var inzien = !isMinimumActie(actie, Actie.AANPASSEN);
		var contactinformatiePanel = new SpecifiekeContactinformatiePanel("specifiekeContactInformatie", model, inzien);
		form.add(contactinformatiePanel);
		var percentageIntegerField = new PercentageIntegerField("fitRetourPercentage", new PropertyModel<Integer>(model, "fitRetourPercentage"), 1);
		percentageIntegerField.setEnabled(!inzien);
		form.add(percentageIntegerField);

		ComponentHelper.addTextField(form, "regioCode", true, 2, String.class, inzien).setOutputMarkupId(true).add(StringValidator.exactLength(2))
			.add(new ScreenitUniqueFieldValidator<>(ScreeningOrganisatie.class, organisatie.getId(), "regioCode", false));
		ComponentHelper.addTextField(form, "rechtbank", false, 255, String.class, inzien);
		ComponentHelper.addTextField(form, "rcmdl", false, 255, String.class, inzien);
		form.add(new TextArea<String>("vertegenwoordiger").add(StringValidator.maximumLength(512)).setEnabled(!inzien));
		form.add(new UploadOrganisatieImageFormComponent("logo", model, UploadOrganisatieImageType.DK_SO_LOGO_INPAKCENTRUM).setEnabled(!inzien));
		form.add(new UploadOrganisatieImageFormComponent("logoBrief", model, UploadOrganisatieImageType.DK_SO_LOGO_BRIEF).setEnabled(!inzien));
		form.add(new UploadOrganisatieImageFormComponent("besturderSign", model, UploadOrganisatieImageType.DK_SO_BESTUUR).setEnabled(!inzien));
		form.add(new UploadOrganisatieImageFormComponent("rcmdlSign", model, UploadOrganisatieImageType.DK_SO_RCMDL).setEnabled(!inzien));
		form.add(new UploadOrganisatieImageFormComponent("kwaliteitslogo", model, UploadOrganisatieImageType.DK_SO_KWALITEITS_LOGO).setEnabled(!inzien));

		var bkGroep = new WebMarkupContainer("borstkankerGegevens");
		form.add(bkGroep);
		bkGroep.add(new GekoppeldeCeBeSePanel("ceBeSeOverzicht", model));
		bkGroep.setVisible(ScreenitSession.get().getOnderzoeken().contains(Bevolkingsonderzoek.MAMMA));

		var ibanField = ComponentHelper.addTextField(form, "iban", true, 34, inzien);
		ibanField.add(new ScreenITIBANValidator());
		ComponentHelper.addTextField(form, "ibanTenaamstelling", true, 70, inzien);

		var allNietGekoppeldeGemeentes = gemeenteService.getNietOfAanScreeningsOrganisatieGekoppeldGemeentes((ScreeningOrganisatie) model.getObject());

		var choices = new SimpleListHibernateModel<Gemeente>(allNietGekoppeldeGemeentes);
		var choiceRenderer = new ChoiceRenderer<Gemeente>("naam", "code")
		{
			@Override
			public Object getDisplayValue(Gemeente object)
			{
				var gemeente = super.getDisplayValue(object);
				if (object.getCode() != null)
				{
					var sb = new StringBuilder();
					sb.append(gemeente);
					sb.append(" (");
					sb.append(object.getCode());
					sb.append(")");
					return sb.toString();
				}
				else
				{
					return gemeente;
				}
			}
		};

		final var gemeentes = new PingPongInput<Gemeente>("gemeentes", new PropertyModel<List<Gemeente>>(model, "gemeentes"), choices, choiceRenderer)
		{
			@Override
			public IModel<Gemeente> model(Gemeente object)
			{
				return ModelUtil.sModel(object);
			}
		};
		gemeentes.setEnabled(!inzien);
		form.add(gemeentes);

		ComponentHelper.addTextField(form, "enovationKlantnummer", false, 10, Integer.class, inzien);
		ComponentHelper.addTextField(form, "enovationEdiAdres", false, 255, String.class, inzien);

		addBmhkLabRetouradressen(form, organisatie, inzien);
		form.add(new AjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var screeningOrganisatie = (ScreeningOrganisatie) model.getObject();
				organisatieService.saveOrUpdateScreeningOrganisatie(screeningOrganisatie, gemeentes.getChoices().getObject(),
					getIngelogdeOrganisatieMedewerker());
				BasePage.markeerFormulierenOpgeslagen(target);
				this.info("Gegevens zijn succesvol opgeslagen");
			}
		});

		var annuleren = new AjaxLink<Medewerker>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}
		};
		form.add(annuleren);
	}

	private void addBmhkLabRetouradressen(ScreenitForm<Organisatie> form, Organisatie organisatie, boolean inzien)
	{
		IModel<List<ZASRetouradres>> retouradressenModel = new PropertyModel<List<ZASRetouradres>>(getDefaultModel(), "retouradressen");
		var retouradressen = retouradressenModel.getObject();

		Map<String, Object> parameters = new HashMap<>();
		parameters.put("actief", true);
		var labs = hibernateService.getByParameters(BMHKLaboratorium.class, parameters);
		for (var lab : labs)
		{
			var found = false;
			for (var retouradres : retouradressen)
			{
				if (retouradres.getLaboratorium().equals(lab))
				{
					found = true;
					break;
				}
			}
			if (!found)
			{
				var retouradres = new ZASRetouradres();
				retouradres.setLaboratorium(lab);
				retouradres.setAdres(new Adres());
				retouradres.setRegio((ScreeningOrganisatie) HibernateHelper.deproxy(organisatie));
				retouradressen.add(retouradres);
			}
		}
		var retouradressenView = new ListView<ZASRetouradres>("retouradressen", retouradressenModel)
		{
			@Override
			protected void populateItem(ListItem<ZASRetouradres> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new Label("laboratorium.naam"));
				item.setVisible(!Boolean.FALSE.equals(item.getModelObject().getLaboratorium().getActief()));
				ComponentHelper.addTextField(item, "adres.huisnummer", false, 10, Integer.class, inzien);
				ComponentHelper.newPostcodeTextField(item, "adres.postcode", false, inzien);
				ComponentHelper.addTextField(item, "adres.plaats", false, 30, inzien);
			}
		};
		var isBMHK = ScreenitSession.get().getOnderzoeken().contains(Bevolkingsonderzoek.CERVIX);
		retouradressenView.setVisible(isBMHK);
		form.add(retouradressenView);
	}
}
