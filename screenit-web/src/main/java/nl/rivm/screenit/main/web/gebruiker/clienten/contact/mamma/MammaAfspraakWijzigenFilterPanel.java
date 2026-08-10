package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.validator.WerkdagValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.preference.service.SimplePreferenceService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidationError;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

import static nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken.MammaAfsprakenBlokPanel.AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER;

public abstract class MammaAfspraakWijzigenFilterPanel extends GenericPanel<MammaAfspraakWijzigenFilter>
{

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseStandplaatsService baseStandplaatsService;

	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private SimplePreferenceService simplePreferenceService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private SimpleListHibernateModel<MammaStandplaats> standplaatsenModel;

	private SimpleListHibernateModel<MammaScreeningsEenheid> screeningsEenhedenModel;

	private final boolean uitstellen;

	private final IModel<MammaStandplaats> standplaats;

	protected MammaAfspraakWijzigenFilterPanel(String id, IModel<MammaAfspraakWijzigenFilter> filterModel, boolean uitstellen, IModel<MammaStandplaats> standplaats)
	{
		super(id, filterModel);
		this.uitstellen = uitstellen;
		this.standplaats = standplaats;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var filter = getModelObject();
		var dossier = filter.getClient().getMammaDossier();

		var form = new Form<MammaAfspraakWijzigenFilter>("form");
		add(form);

		var vanafField = addVanafField(form, dossier);
		var totEnMetField = addTotEnMetField(form, dossier);

		if (!uitstellen)
		{
			form.add(new DependantDateValidator(vanafField, totEnMetField, DependantDateValidator.Operator.AFTER));
		}

		resetFilter(null);
		List<MammaVerzettenReden> values = new ArrayList<>(Arrays.asList(MammaVerzettenReden.values()));
		values.remove(MammaVerzettenReden.CLIENTEN_PORTAAL);
		values.remove(MammaVerzettenReden.PASSANT);

		var panelCreateContext = getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY);
		if (dossier.getDoelgroep().equals(MammaDoelgroep.MINDERVALIDE) || dossier.getTehuis() != null || panelCreateContext.bkAlleenClientContact)
		{
			values.removeAll(MammaVerzettenReden.BRIEF_VERPLICHT);
			filter.setVerzettenReden(MammaVerzettenReden.CLIENT_CONTACT);
		}
		var vanuitPlanningOfAfsprakenkalender =
			panelCreateContext.bkVanuitPlanning || ScreenitSession.get().isZoekObjectGezetForComponent(AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER);
		if (!vanuitPlanningOfAfsprakenkalender)
		{
			filter.setVerzettenReden(MammaVerzettenReden.CLIENT_CONTACT);
		}
		var reden = new RadioChoice<MammaVerzettenReden>("verzettenReden", values, new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setRequired(true);

		var redenContainer = new WebMarkupContainer("redenContainer");
		redenContainer.setOutputMarkupPlaceholderTag(true);
		redenContainer.add(reden);

		redenContainer.setVisible(vanuitPlanningOfAfsprakenkalender && !uitstellen && values.size() > 1);
		form.add(redenContainer);

		var standplaatsenContainer = new WebMarkupContainer("standplaatsenContainer");
		form.add(standplaatsenContainer);
		standplaatsenContainer.add(new ScreenitListMultipleChoice<>("standplaatsen", standplaatsenModel, new ChoiceRenderer<>("naam")));
		standplaatsenContainer.setOutputMarkupId(true);

		var screeningsEenhedenContainer = new WebMarkupContainer("screeningsEenhedenContainer");
		form.add(screeningsEenhedenContainer);
		screeningsEenhedenContainer.add(new ScreenitListMultipleChoice<>("screeningsEenheden", screeningsEenhedenModel, new ChoiceRenderer<>("naam")));
		screeningsEenhedenContainer.setOutputMarkupId(true);

		form.add(new CheckBox("extraOpties").setVisible(!uitstellen));

		var afstanden = new Integer[] { 5, 10, 15, 20, 25, 30, 35, 40, 45 };
		var afstandField = new ScreenitDropdown<Integer>("afstand", Arrays.asList(afstanden));
		afstandField.setNullValid(true);
		form.add(afstandField);

		var buitenRegioField = new CheckBox("buitenRegio");
		buitenRegioField.setVisible(ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_BUITEN_REGIO, Actie.AANPASSEN));
		form.add(buitenRegioField);

		buitenRegioField.add(maakChangeBehavior(form));
		maakZoekButtons(form);
	}

	private DatePicker<Date> addVanafField(Form<MammaAfspraakWijzigenFilter> form, MammaDossier dossier)
	{
		var minimaleVanaf = dateSupplier.getLocalDate();
		var minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());

		var vroegstMogelijkeUitnodigingsDatum = baseAfspraakService.vroegstMogelijkeUitnodigingsDatum(dossier, minimaleVanaf, minimaleIntervalMammografieOnderzoeken);
		var momenteelBinnenHonderdTachtigRegel = !minimaleVanaf.isEqual(vroegstMogelijkeUitnodigingsDatum);

		var vanafField = ComponentHelper.newDatePicker("vanaf");
		form.add(vanafField);
		vanafField.setRequired(true);

		vanafField.add(new DateValidator()
		{
			@Override
			public Date getMinimum()
			{
				return DateUtil.toUtilDate(vroegstMogelijkeUitnodigingsDatum);
			}

			@Override
			protected IValidationError decorate(IValidationError error, IValidatable<Date> validatable)
			{
				var validationError = (ValidationError) super.decorate(error, validatable);
				if (momenteelBinnenHonderdTachtigRegel)
				{
					validationError.setKeys(List.of("vanafdatum.binnen.hondertachtig.dagen"));
					validationError.setVariable("dagen", preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.toString()));
				}
				return validationError;
			}
		});

		if (uitstellen)
		{
			vanafField.add(new WerkdagValidator());
		}
		return vanafField;
	}

	private DatePicker<Date> addTotEnMetField(Form<MammaAfspraakWijzigenFilter> form, MammaDossier dossier)
	{
		var totEnMetField = ComponentHelper.newDatePicker("totEnMet");

		form.add(totEnMetField);
		totEnMetField.setRequired(true);
		var laatstMogelijkeUitnodigingsDatum = baseAfspraakService.laatstMogelijkeAfspraakDatum(dossier);
		if (laatstMogelijkeUitnodigingsDatum != null)
		{
			totEnMetField.add(DateValidator.maximum(DateUtil.toUtilDate(laatstMogelijkeUitnodigingsDatum)));
		}
		totEnMetField.setVisible(!uitstellen);
		return totEnMetField;
	}

	private void maakZoekButtons(Form<MammaAfspraakWijzigenFilter> form)
	{
		form.add(new IndicatingAjaxSubmitLink("zoeken")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				onZoeken(target);
			}

		});
	}

	private void onZoeken(AjaxRequestTarget target)
	{
		var filter = getModelObject();
		if (MammaVerzettenReden.briefVerplicht(filter.getVerzettenReden()))
		{
			int aantalWerkdagenVerzettenVanaf = simplePreferenceService
				.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name());
			var minDatumBriefVerplicht = DateUtil.toLocalDate(DateUtil.plusWerkdagen(dateSupplier.getDateMidnight(), aantalWerkdagenVerzettenVanaf));
			if (minDatumBriefVerplicht.isAfter(filter.getVanaf()))
			{
				error("Bij de gekozen reden moet een brief gestuurd worden. Daarom moet de minimale 'Vanaf' datum op of na "
					+ DateTimeFormatter.ofPattern("dd-MM-yyyy").format(minDatumBriefVerplicht) + " liggen");
			}
		}
		if (!hasErrorMessage())
		{
			zoeken(target);
		}
	}

	private AjaxFormComponentUpdatingBehavior maakChangeBehavior(WebMarkupContainer form)
	{
		return new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetFilter(target);
				target.add(form);
			}
		};
	}

	private void resetFilter(AjaxRequestTarget target)
	{
		ScreeningOrganisatie screeningOrganisatie = null;
		var filter = getModelObject();
		var buitenRegio = filter.isBuitenRegio();
		if (!buitenRegio)
		{
			screeningOrganisatie = filter.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		}

		var standplaatsen = baseStandplaatsService.getActieveStandplaatsen(screeningOrganisatie);
		var screeningsEenheden = screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(screeningOrganisatie);
		if (!buitenRegio)
		{
			var savedFilterScreeninsEenheden = filter.getScreeningsEenheden();
			var savedFilterStandplaatsen = filter.getStandplaatsen();
			filter.setStandplaatsen(new ArrayList<>());
			filter.setScreeningsEenheden(new ArrayList<>());
			standplaatsen = Stream.of(standplaatsen, standplaatsPeriodeService.getStandplaatsenBuitenRegio(filter, uitstellen))
				.flatMap(Collection::stream)
				.distinct()
				.sorted(Comparator.comparing(MammaStandplaats::getNaam))
				.collect(Collectors.toList());
			screeningsEenheden = Stream.of(screeningsEenheden, standplaatsPeriodeService.getScreeningEenhedenBuitenRegio(filter, uitstellen))
				.flatMap(Collection::stream)
				.distinct()
				.sorted(Comparator.comparing(MammaScreeningsEenheid::getCode))
				.collect(Collectors.toList());
			filter.setStandplaatsen(savedFilterStandplaatsen);
			filter.setScreeningsEenheden(savedFilterScreeninsEenheden);
		}
		if (target == null)
		{
			standplaatsenModel = ModelUtil.listRModel(standplaatsen);
			screeningsEenhedenModel = ModelUtil.listRModel(screeningsEenheden);
		}
		else
		{
			standplaatsenModel.setObject(standplaatsen);
			screeningsEenhedenModel.setObject(screeningsEenheden);
		}
	}

	protected abstract void zoeken(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(standplaatsenModel);
		ModelUtil.nullSafeDetach(screeningsEenhedenModel);
		ModelUtil.nullSafeDetach(standplaats);
	}
}
