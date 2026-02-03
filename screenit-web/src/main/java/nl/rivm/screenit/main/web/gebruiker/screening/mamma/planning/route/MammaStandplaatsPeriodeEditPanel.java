package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.exception.MagOpslaanException;
import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.AttributeRemover;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.wicketstuff.wiquery.core.javascript.JsStatement;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class MammaStandplaatsPeriodeEditPanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{
	@SpringBean
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@SpringBean
	private MammaRouteService routeService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean(name = "applicationUrl")
	private String applicationUrl;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private final LocalDate initieleVanaf;

	private final LocalDate initieleTotEnMet;

	private final LocalDate vrijgegevenTotEnMet;

	private final LocalDate uitnodigenTotEnMet;

	private final IModel<Date> nieuweTotEnMetDatumModel = new Model<>();

	private final IModel<Boolean> isPrognoseModel;

	private final ScreenitDropdown<Long> achtervangStandplaatsPeriodeDropdown;

	private final WebMarkupContainer achtervangStandplaatsContainer;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/util.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/data.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/rij.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/matrix.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/voorspellingsgrafiek/voorspellingsgrafiek.js"));

		var jsStatement = new JsStatement();
		jsStatement.append("deVoorspellingsgrafiekenDeelnamekans = new VoorspellingsgrafiekenDeelnamekans();");
		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}

	protected MammaStandplaatsPeriodeEditPanel(String id, IModel<PlanningStandplaatsPeriodeDto> model, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode,
		IModel<MammaScreeningsEenheid> screeningsEenheidModel, boolean magBeginDatumWijzigen, boolean magEindDatumWijzigen)
	{
		super(id, model);

		final var standplaatsPeriodeDto = model.getObject();

		var ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		var magAanpassen = ingelogdNamensRegio != null && ScreenitSession.get().checkPermission(Recht.MEDEWERKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);

		var actieveStandplaatsen = getStandplaatsen(ingelogdNamensRegio);

		var form = new ScreenitForm<MammaStandplaatsPeriode>("form");
		add(form);

		final var accordionContainer = new WebMarkupContainer("accordionContainer");

		var standVanZakenContainer = new WebMarkupContainer("standVanZakenContainer");
		var standVanZakenLink = new WebMarkupContainer("standVanZakenLink");
		addLinkBehaviorAccordion(accordionContainer, standVanZakenContainer, standVanZakenLink);

		var configurationContainer = new WebMarkupContainer("configurationContainer");
		var configurationLink = new WebMarkupContainer("configurationLink");
		addLinkBehaviorAccordion(accordionContainer, configurationContainer, configurationLink);

		var afspraakDrempelContainer = new WebMarkupContainer("afspraakDrempelContainer");
		var afspraakDrempelLink = new WebMarkupContainer("afspraakDrempelLink");
		addLinkBehaviorAccordion(accordionContainer, afspraakDrempelContainer, afspraakDrempelLink);

		var standplaats = hibernateService.get(MammaStandplaats.class, standplaatsPeriodeDto.standplaatsId);

		var naam = StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(getModelObject(), standplaats);

		form.add(new Label("standplaatsRonde.standplaats.naam", naam));

		afspraakDrempelContainer.add(new Label("standplaatsRonde.standplaats.regio.afspraakDrempelBk", standplaats.getRegio().getAfspraakDrempelBk()));

		var afspraakDrempel = new TextField<Integer>("afspraakDrempel");
		afspraakDrempelContainer.add(afspraakDrempel);
		afspraakDrempel.setEnabled(magAanpassen);
		afspraakDrempel.setRequired(false);
		afspraakDrempel.setType(Integer.class);
		afspraakDrempel.add(RangeValidator.range(0, 100));

		DatePicker<Date> einddatumPicker;
		var prognose = Boolean.TRUE.equals(standplaatsPeriodeDto.prognose);
		nieuweTotEnMetDatumModel.setObject(DateUtil.toUtilDate(standplaatsPeriodeDto.totEnMet));
		einddatumPicker = ComponentHelper.newYearDatePicker("totEnMet", nieuweTotEnMetDatumModel);
		einddatumPicker.setOutputMarkupId(true);
		einddatumPicker.setDisabled(!magEindDatumWijzigen || !magAanpassen);
		configurationContainer.add(einddatumPicker);

		isPrognoseModel = Model.of(prognose);
		var totEnMetPrognoseCheckbox = new AjaxCheckBox("totEnMetPrognoseCheckbox", isPrognoseModel)
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				updateEindDatumOnPrognose(getModelObject(), einddatumPicker, magEindDatumWijzigen, magAanpassen);
				target.add(einddatumPicker, achtervangStandplaatsContainer);
			}
		};
		totEnMetPrognoseCheckbox.setEnabled(magEindDatumWijzigen && magAanpassen);

		configurationContainer.add(totEnMetPrognoseCheckbox);

		form.setEnabled(true);

		var vanafDatum = ComponentHelper.newYearDatePicker("vanaf");
		if (magBeginDatumWijzigen)
		{
			configurationContainer.add(new EmptyPanel("vanafLabel"));
		}
		else
		{
			vanafDatum.setVisible(false);
			configurationContainer.add(new Label("vanafLabel", getModelObject().vanaf));
		}
		vanafDatum.setOutputMarkupId(true);
		vanafDatum.setDisabled(!magAanpassen);
		configurationContainer.add(vanafDatum);

		achtervangStandplaatsPeriodeDropdown = new ScreenitDropdown<>("achtervangStandplaatsId",
			actieveStandplaatsen, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Long object)
			{
				return hibernateService.load(MammaStandplaats.class, object).getNaam();
			}
		});
		achtervangStandplaatsPeriodeDropdown.setOutputMarkupId(true);
		achtervangStandplaatsPeriodeDropdown.setEnabled(magAanpassen);

		achtervangStandplaatsContainer = new WebMarkupContainer("achtervangStandplaatsContainer");
		achtervangStandplaatsContainer.setOutputMarkupId(true);
		achtervangStandplaatsContainer.add(achtervangStandplaatsPeriodeDropdown);
		configurationContainer.add(achtervangStandplaatsContainer);

		einddatumPicker.add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				achtervangStandplaatsPeriodeDropdown.setEnabled(einddatumPicker.getModelObject() != null);
				if (einddatumPicker.getModelObject() == null)
				{
					getModelObject().achtervangStandplaatsId = null;
				}
				ajaxRequestTarget.add(achtervangStandplaatsContainer);
			}
		});

		if (getModelObject().prognose)
		{
			achtervangStandplaatsPeriodeDropdown.setEnabled(false);
		}
		updateEindDatumOnPrognose(prognose, einddatumPicker, magEindDatumWijzigen, magAanpassen);

		var mindervalideUitnodigenVanaf = ComponentHelper.newYearDatePicker("mindervalideUitnodigenVanaf");
		mindervalideUitnodigenVanaf.setDisabled(!magAanpassen);
		configurationContainer.add(mindervalideUitnodigenVanaf);

		addAfspraakcapaciteitBeschikbaarVoor(configurationContainer, standplaats);

		initieleVanaf = getModelObject().vanaf;
		initieleTotEnMet = getModelObject().totEnMet;

		var screeningsEenheid = screeningsEenheidModel.getObject();
		vrijgegevenTotEnMet = DateUtil.toLocalDate(screeningsEenheid.getVrijgegevenTotEnMet());
		uitnodigenTotEnMet = DateUtil.toLocalDate(screeningsEenheid.getUitnodigenTotEnMet());

		var geenCapaciteitOverzicht = new WebMarkupContainer("geenCapaciteitOverzicht");
		standVanZakenContainer.add(geenCapaciteitOverzicht);
		var capaciteitOverzichtContainer = new WebMarkupContainer("capaciteitOverzichtContainer");
		standVanZakenContainer.add(capaciteitOverzichtContainer);

		var standplaatsPeriodeId = standplaatsPeriodeDto.id;
		MammaStandplaatsPeriode standplaatsPeriode = null;
		if (standplaatsPeriodeId != null)
		{
			standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeId);
		}
		if (standplaatsPeriode != null && vrijgegevenTotEnMet != null && !vrijgegevenTotEnMet.isBefore(standplaatsPeriodeDto.vanaf))
		{
			geenCapaciteitOverzicht.setVisible(false);

			var vandaag = dateSupplier.getLocalDate();

			var vanaf = Collections.max(Arrays.asList(vandaag, standplaatsPeriodeDto.vanaf));
			var totEnMet = Collections.min(Arrays.asList(vrijgegevenTotEnMet, standplaatsPeriodeDto.totEnMet));

			var capaciteit = baseCapaciteitsBlokService.getCapaciteit(baseCapaciteitsBlokService
				.getNietGeblokkeerdeScreeningCapaciteitBlokDtos(standplaatsPeriode, DateUtil.toUtilDate(vanaf.atStartOfDay()),
					DateUtil.toUtilDate(totEnMet.atTime(Constants.BK_EINDTIJD_DAG)), null));

			var beschikbareCapaciteitRegulier = capaciteit.getBeschikbareCapaciteit();
			var vrijeCapaciteitRegulier = capaciteit.getVrijeCapaciteit();

			capaciteitOverzichtContainer.add(new Label("beschikbaarRegulier",
				beschikbareCapaciteitRegulier.setScale(0, RoundingMode.HALF_UP).toString()));
			capaciteitOverzichtContainer.add(new Label("vrijRegulier",
				vrijeCapaciteitRegulier.setScale(1, RoundingMode.HALF_UP).toString()));
		}
		else
		{
			capaciteitOverzichtContainer.setVisible(false);
		}

		var nietUitgenodigd = new WebMarkupContainer("nietUitgenodigd");
		standVanZakenContainer.add(nietUitgenodigd);
		var uitnodigenOverzichtContainer = new WebMarkupContainer("uitnodigenOverzichtContainer");
		standVanZakenContainer.add(uitnodigenOverzichtContainer);

		MammaStandplaatsRondeUitnodigenRapportage standplaatsRondeUitnodigenRapportage = null;
		if (standplaatsPeriode != null)
		{
			standplaatsRondeUitnodigenRapportage = standplaatsPeriodeService.getStandplaatsRondeUitnodigenRapportage(standplaatsPeriode.getStandplaatsRonde());
		}

		if (standplaatsRondeUitnodigenRapportage != null && standplaatsRondeUitnodigenRapportage.getStatus() != MammaStandplaatsRondeRapportageStatus.ALLEEN_UITSTEL_UITNODIGINGEN)
		{
			nietUitgenodigd.setVisible(false);

			uitnodigenOverzichtContainer.setDefaultModel(ModelUtil.csModel(standplaatsRondeUitnodigenRapportage));
			uitnodigenOverzichtContainer.add(new Label("totaalTotaal"));
			uitnodigenOverzichtContainer.add(new Label("totaalVervolgRonde"));
			uitnodigenOverzichtContainer.add(new Label("totaalEersteRonde"));
			uitnodigenOverzichtContainer.add(new Label("totaalDubbeleTijd"));
			uitnodigenOverzichtContainer.add(new Label("totaalMindervalide"));
			uitnodigenOverzichtContainer.add(new Label("totaalTehuis"));
			uitnodigenOverzichtContainer.add(new Label("totaalSuspect"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenTotaal"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenVervolgRonde"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenEersteRonde"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenDubbeleTijd"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenMindervalide"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenTehuis"));
			uitnodigenOverzichtContainer.add(new Label("uitTeNodigenSuspect"));
		}
		else
		{
			uitnodigenOverzichtContainer.setVisible(false);
		}

		var splitsenKnop = new IndicatingAjaxButton("splitsen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (!magSplitsen())
				{
					standVanZakenContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					afspraakDrempelContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeAppender("class", " in"));
					target.add(configurationContainer);

					return;
				}

				standplaatsPeriodeService.splitsStandplaatsPeriode(MammaStandplaatsPeriodeEditPanel.this.getModelObject(),
					ScreenitSession.get().getIngelogdeOrganisatieMedewerker());

				info(getString("message.gegevens.onthouden"));
				standplaatsPeriodeGewijzigd(target);
			}
		};
		splitsenKnop.setEnabled(magAanpassen);
		form.add(splitsenKnop);

		var dialog = new BootstrapDialog("dialog");
		add(dialog);

		ConfirmingIndicatingAjaxSubmitLink<Void> opslaanKnop = new ConfirmingIndicatingAjaxSubmitLink<>("opslaan", dialog, null)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					var nieuweEindDatum = DateUtil.toLocalDate(einddatumPicker.getModelObject());
					var nieuwePrognose = isPrognoseModel.getObject();
					standplaatsPeriodeService.magOnthouden(standplaatsPeriodeDto, nieuwePrognose, nieuweEindDatum, vrijgegevenTotEnMet, uitnodigenTotEnMet,
						volgendeStandplaatsPeriode);

					standplaatsPeriodeDto.prognose = isPrognoseModel.getObject();
					standplaatsPeriodeDto.totEnMet = nieuweEindDatum;

					var isStandplaatsGewijzigd = opslaan();
					if (isStandplaatsGewijzigd)
					{
						info(getString("message.gegevens.onthouden"));
						standplaatsPeriodeGewijzigd(target);
					}
				}
				catch (MagOpslaanException ex)
				{
					error(String.format(getString(ex.getMessageKey()), ex.getFormatArguments()));
					standVanZakenContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					afspraakDrempelContainer.add(new AttributeRemover("class", Model.of(" in"), " "));
					configurationContainer.add(new AttributeAppender("class", " in"));
					target.add(configurationContainer);
				}
			}

			@Override
			protected boolean skipConfirmation()
			{
				return !afsprakenWordenVerzet();
			}

			private boolean afsprakenWordenVerzet()
			{

				var nieuweEindDatum = DateUtil.toLocalDate(einddatumPicker.getModelObject());
				var screeningsEenheid = screeningsEenheidModel.getObject();
				var aantalAfspraken = standplaatsPeriodeService.countAfsprakenTeVerzetten(nieuweEindDatum, standplaatsPeriodeDto, screeningsEenheid);
				return aantalAfspraken > 0;
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				if (afsprakenWordenVerzet())
				{
					return Model.of(getAfsprakenVerzetMelding());
				}
				return Model.of("");
			}

			private String getAfsprakenVerzetMelding()
			{
				var nieuweEindDatum = DateUtil.toLocalDate(einddatumPicker.getModelObject());
				var nieuweEindDatumText = nieuweEindDatum.format(DateUtil.LOCAL_DATE_FORMAT);
				var screeningsEenheid = screeningsEenheidModel.getObject();
				var aantalAfspraken = standplaatsPeriodeService.countAfsprakenTeVerzetten(nieuweEindDatum, standplaatsPeriodeDto, screeningsEenheid);
				var volgendeStandplaats = hibernateService.get(MammaStandplaatsPeriode.class, volgendeStandplaatsPeriode.id);
				var volgendeStandplaatsNaam = "";
				if (volgendeStandplaats != null)
				{
					volgendeStandplaatsNaam = volgendeStandplaats.getStandplaatsRonde().getStandplaats().getNaam();
				}

				return String.format(getString("Standplaatsperiode.einddatum.veranderen.afspraken.worden.verzet"), nieuweEindDatumText, aantalAfspraken,
					volgendeStandplaatsNaam);
			}

			@Override
			protected IModel<String> getHeaderStringModel()
			{
				return Model.of(getString("Standplaatsperiode.opslaan.validatie.waarschuwing.header"));
			}
		};

		opslaanKnop.setEnabled(magAanpassen);
		form.add(opslaanKnop);
		form.setDefaultButton(opslaanKnop);

		form.add(new AjaxLink<Void>("close")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});

		afspraakDrempelContainer.add(new HiddenField<>("applicationUrl", Model.of(applicationUrl)));
		afspraakDrempelContainer.add(new HiddenField<>("subUrl", Model.of("/api/getAfspraakDrempelOverzichtStandplaats?standplaatsId=" + standplaats.getId() + "&")));

		accordionContainer.add(afspraakDrempelLink);
		accordionContainer.add(configurationLink);
		accordionContainer.add(standVanZakenLink);
		accordionContainer.add(afspraakDrempelContainer);
		accordionContainer.add(configurationContainer);
		accordionContainer.add(standVanZakenContainer);

		form.add(accordionContainer);
	}

	private void updateEindDatumOnPrognose(boolean prognose, DatePicker<Date> einddatumPicker, boolean magEindDatumWijzigen, boolean magAanpassen)
	{
		if (!magEindDatumWijzigen || !magAanpassen)
		{
			return;
		}

		if (prognose)
		{
			einddatumPicker.setModelObject(DateUtil.toUtilDate(getModelObject().totEnMet));
		}
		einddatumPicker.setEnabled(!prognose);
		einddatumPicker.setDisabled(prognose);
		achtervangStandplaatsPeriodeDropdown.setEnabled(!prognose);
	}

	private List<Long> getStandplaatsen(ScreeningOrganisatie ingelogdNamensRegio)
	{
		List<Long> standplaatsenIds = new ArrayList<>();
		if (ingelogdNamensRegio != null)
		{
			var standplaatsen = routeService.getStandplaatsenMetRoute(ingelogdNamensRegio);
			for (var id : getModelObject().afspraakcapaciteitBeschikbaarVoorIds)
			{
				standplaatsen.addAll(routeService.getStandplaatsenMetRoute(hibernateService.load(ScreeningOrganisatie.class, id)));
			}
			standplaatsenIds = standplaatsen.stream()
				.filter(standplaats -> !standplaats.getId().equals(getModelObject().standplaatsId)).sorted(Comparator.comparing(MammaStandplaats::getNaam))
				.map(MammaStandplaats::getId)
				.collect(Collectors.toList());
		}
		return standplaatsenIds;
	}

	private void addAfspraakcapaciteitBeschikbaarVoor(WebMarkupContainer configurationContainer, MammaStandplaats standplaats)
	{
		var alleScreeningorganisatieIds = organisatieService.getAllActiefScreeningOrganisaties().stream().map(ScreeningOrganisatie::getId).collect(Collectors.toList());
		alleScreeningorganisatieIds.remove(standplaats.getRegio().getId());
		configurationContainer.add(new ScreenitListMultipleChoice<>("afspraakcapaciteitBeschikbaarVoorIds", alleScreeningorganisatieIds, new ChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Long object)
			{
				return hibernateService.load(ScreeningOrganisatie.class, object).getNaam();
			}
		}).add(new OnChangeAjaxBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				var standplaatsenIds = getStandplaatsen(ScreenitSession.get().getScreeningOrganisatie());
				if (!standplaatsenIds.contains(getModelObject().achtervangStandplaatsId))
				{
					getModelObject().achtervangStandplaatsId = null;
				}
				achtervangStandplaatsPeriodeDropdown.setChoices(standplaatsenIds);
				ajaxRequestTarget.add(achtervangStandplaatsContainer);
			}
		}));
	}

	private boolean magSplitsen()
	{
		var standplaatsPeriodeDto = getModelObject();

		var nieuweTotEnMet = DateUtil.toLocalDate(nieuweTotEnMetDatumModel.getObject());
		boolean oudePrognose = standplaatsPeriodeDto.prognose;
		var nieuwePrognose = Boolean.TRUE.equals(isPrognoseModel.getObject());

		if (nieuwePrognose)
		{
			error(getString("Standplaatsperiode.splitsen.met.prognose"));
			return false;
		}

		if (!initieleVanaf.equals(standplaatsPeriodeDto.vanaf) || !initieleTotEnMet.equals(nieuweTotEnMet) || oudePrognose)
		{
			error(getString("Standplaatsperiode.onthoud.wijzigingen"));
			return false;
		}

		if (Boolean.TRUE.equals(standplaatsPeriodeDto.gesplitst))
		{
			error(getString("Standplaatsperiode.is.al.gesplitst"));
			return false;
		}
		return true;
	}

	private boolean opslaan()
	{
		return standplaatsPeriodeService.saveOrUpdateStandplaatsPeriode(getModelObject(),
			ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
	}

	private void addLinkBehaviorAccordion(WebMarkupContainer accordionContainer, WebMarkupContainer teOpenenContainer, WebMarkupContainer link)
	{

		link.add(new AttributeAppender("data-parent", "#" + accordionContainer.getMarkupId()));

		link.add(new AttributeAppender("href", "#" + teOpenenContainer.getMarkupId()));
	}

	protected abstract void close(AjaxRequestTarget target);

	protected abstract void standplaatsPeriodeGewijzigd(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		isPrognoseModel.detach();
		nieuweTotEnMetDatumModel.detach();
	}
}
