package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen;

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

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.MammaImsUserSessionType;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.service.mamma.MammaLezingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaBiradsKeuzePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaLezingParameters;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaLaesiesAfbeeldingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen.popup.MammaBeoordelenPreLezingOpslaanDialoog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.mamma.imsapi.model.FhirUserSession;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.CallbackParameter;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaBeoordelenLezingPanel extends AbstractBEAccordionPanel<MammaLezing>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBeoordelenLezingPanel.class);

	private static final String MAMMA_BRIDGE_IMAGES_SEEN_RESPONSE_OBJECT_STRING = "fhircontext";

	private final MammaBeoordelenHuidigeRondePanel beoordelingPanel;

	private IModel<List<LaesieDto>> laesieDtos;

	@SpringBean
	private MammaImsService imsService;

	private AbstractDefaultAjaxBehavior opslaanCallback;

	private MammaBeoordelenPreLezingOpslaanDialoog beoordelenPreLezingOpslaanDialoog;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaLezingService lezingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private IndicatingAjaxButton formSubmitBtn;

	@SpringBean
	private LogService logService;

	private MammaBIRADSWaarde prePopupBiradsWaardeLinks;

	private MammaBIRADSWaarde prePopupBiradsWaardeRechts;

	public MammaBeoordelenLezingPanel(MammaBeoordelenHuidigeRondePanel beoordelingPanel, String id, IModel<MammaLezing> lezingModel)
	{
		super(id, lezingModel, Model.of("BI-RADS beoordeling"), 12);
		this.beoordelingPanel = beoordelingPanel;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		createBeoordelingFormulier(getModel(), panelContainer);
		dialoogOpslaanToevoegen(beoordelingPanel);
	}

	private void createBeoordelingFormulier(IModel<MammaLezing> lezingModel, WebMarkupContainer content)
	{
		var lezingParameters = maakLezingParameters(getModelObject());

		var form = new ScreenitForm<MammaLezing>("birads_form");

		var beperktBeoordeelbaarPanel = new BeperktBeoordeelbaarPanel("beperktBeoordeelbaarPanel", lezingModel, false);

		form.add(new MammaBiradsKeuzePanel("biradskeuze", lezingModel, lezingParameters)
		{
			@Override
			public void onBiradsKeuzeChange(AjaxRequestTarget target)
			{
				beperktBeoordeelbaarPanel.updateOnbeoordeelbaar(target);
			}
		});

		var biradsOpmerkingContainer = new WebMarkupContainer("biradsOpmerkingContainer");
		form.add(biradsOpmerkingContainer);
		var opmerkingTextArea = new TextArea<String>("biradsOpmerking");
		tuneTextArea(biradsOpmerkingContainer, opmerkingTextArea, lezingParameters.isInzien(), false);

		createOpschortVelden(lezingParameters, form);

		form.add(beperktBeoordeelbaarPanel);

		form.add(new NevenbevindingenBeoordelingPanel("nevenbevindingenPanel", lezingModel, beoordelingPanel.getModel(), false));

		createRedenenFotobesprekingVelden(form, lezingParameters.isInzien());

		form.add(maakTomosyntheseRelevantRadioChoice(lezingParameters));

		createOpslaanCallbackAjaxBehavior();

		var mapper = new LaesieDtoMapper();
		laesieDtos = new ListModel<>(mapper.lezingToLaesieDtos(lezingModel.getObject()));
		var amputatie = beoordelingPanel.getModelObject().getOnderzoek().getAmputatie();
		form.add(new MammaLaesiesAfbeeldingPanel("afbeelding", laesieDtos, false, lezingModel.getObject().getId(), amputatie));

		content.add(form);

		formSubmitBtn = new ScreenitIndicatingAjaxButton("formSubmit", form)
		{
			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				var myAjaxCallListener = new AjaxCallListener();
				myAjaxCallListener.onBefore("logOnAfrondenClick();");
				attributes.getAjaxCallListeners().add(myAjaxCallListener);
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (!beoordelingService.isLezingValide(lezingModel.getObject(), laesieDtos.getObject()))
				{
					error(getString("error.lezing.annotatie.en.birads"));
					return;
				}
				if (!beoordelingPanel.isOpslaanPopupGezien())
				{
					target.appendJavaScript(String.format("sendAllImagesSeenRequest('%s',%s,%s)", MammaImsUserSessionType.AllImagesSeen,
						beoordelingPanel.getModelObject().getOnderzoek().getId(),
						opslaanCallback.getCallbackFunction(CallbackParameter.explicit(MAMMA_BRIDGE_IMAGES_SEEN_RESPONSE_OBJECT_STRING))));
				}
				else
				{
					var lezing = MammaBeoordelenLezingPanel.this.getModelObject();

					var beoordeling = beoordelingPanel.getModelObject();
					if (heeftAfwijkingen()
						&& (MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordeling.getStatus()))
						&& lezing.getBeperktBeoordeelbaarReden() == null
						&& MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN.equals(beoordeling.getOpschortReden()))
					{
						var client = baseBeoordelingService.getClientVanBeoordeling(beoordelingPanel.getModelObject());
						lezingService.logPopupPreBirads(client, ScreenitSession.get().getIngelogdeOrganisatieMedewerker(), lezing,
							prePopupBiradsWaardeLinks, prePopupBiradsWaardeRechts);
					}
					beoordelingPanel.lezingOpslaan(MammaBeoordelenLezingPanel.this.getModel(), target, laesieDtos.getObject());
				}
			}

		};
		formSubmitBtn.setOutputMarkupId(true);
		formSubmitBtn.setVisible(!lezingParameters.isInzien() && ScreenitSession.get().checkPermission(Recht.MEDEWERKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN));
		form.add(formSubmitBtn);

	}

	private RadioChoice<Boolean> maakTomosyntheseRelevantRadioChoice(MammaLezingParameters lezingParameters)
	{
		var radioChoice = ComponentHelper.addHorizontaleBooleanRadioChoice("tomosyntheseRelevantVoorBeoordeling");
		radioChoice.setSuffix("<span class=\"checkmark\"></span></label>");

		radioChoice.setRequired(true);
		radioChoice.setEnabled(!lezingParameters.isInzien());
		radioChoice.setVisible(lezingParameters.isToonTomosyntheseSlicesRadioButtons());

		return radioChoice;
	}

	public static void tuneTextArea(WebMarkupContainer container, TextArea<String> textArea, boolean alleenInzien, boolean required)
	{
		textArea.setEnabled(!alleenInzien);
		textArea.setRequired(required);
		textArea.setOutputMarkupId(true);
		textArea.setVisible(true);
		textArea.add(StringValidator.maximumLength(HibernateMagicNumber.L255));
		container.add(textArea);
	}

	private void createOpschortVelden(MammaLezingParameters lezingParameters, ScreenitForm<MammaLezing> form)
	{
		var opschortRedenContainer = new WebMarkupContainer("opschortRedenContainer");
		opschortRedenContainer.setOutputMarkupPlaceholderTag(true);
		opschortRedenContainer.setOutputMarkupId(true);
		var ronde = beoordelingPanel.getModelObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde();
		var opgeschorteBeoordeling = baseBeoordelingService.zoekOpgeschorteBeoordelingInRonde(ronde,
			MammaBeoordelingOpschortenReden.AANVULLENDE_BEELDEN_NODIG_SE, MammaBeoordelingOpschortenReden.PRIORS_VAN_BUITEN_BVO);
		opschortRedenContainer.setEnabled(opgeschorteBeoordeling.isEmpty());
		form.add(opschortRedenContainer);

		var opschortRedenTekstLabel = new Label("prevOpschortRedenTekst", opgeschorteBeoordeling.map(MammaBeoordeling::getOpschortRedenTekst).orElse(null));
		opschortRedenTekstLabel.setVisible(opgeschorteBeoordeling.isPresent());
		opschortRedenContainer.add(opschortRedenTekstLabel);

		var opschortRedenTekstContainer = new WebMarkupContainer("opschortRedenTekstContainer");
		opschortRedenTekstContainer.setOutputMarkupPlaceholderTag(true);
		opschortRedenTekstContainer.setOutputMarkupId(true);
		opschortRedenTekstContainer.setVisible(false);
		opschortRedenContainer.add(opschortRedenTekstContainer);

		var beoordeling = beoordelingPanel.getModelObject();

		var opschortRedenen = beoordelingService.getMogelijkeOpschortRedenen(beoordeling, getModelObject().getLezingType());
		var opschortenReden = new ScreenitDropdown<MammaBeoordelingOpschortenReden>("opschortReden",
			new CompoundPropertyModel<>(new PropertyModel<>(beoordelingPanel.getModel(), "opschortReden")), opschortRedenen, new EnumChoiceRenderer<>());

		var isNietInzien = !lezingParameters.isInzien();
		var alleenNietOpschortenMogelijk = opschortRedenen.size() == 1 && opschortRedenen.get(0) == MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN;

		opschortenReden.setEnabled(isNietInzien && !alleenNietOpschortenMogelijk);
		opschortRedenContainer.add(opschortenReden);

		opschortenReden.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				var beoordeling = beoordelingPanel.getModelObject();
				var beoordelingNietOpschorten = beoordeling.getOpschortReden().equals(MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN);
				opschortRedenTekstContainer.setVisible(isNietInzien && !beoordelingNietOpschorten);
				if (beoordelingNietOpschorten)
				{
					beoordeling.setOpschortRedenTekst(null);
				}
				target.add(opschortRedenTekstContainer);
			}
		});

		var opschortenRedenTekst = new TextArea<String>("opschortRedenTekst",
			new CompoundPropertyModel<>(new PropertyModel<>(beoordelingPanel.getModel(), "opschortRedenTekst")));
		tuneTextArea(opschortRedenTekstContainer, opschortenRedenTekst, lezingParameters.isInzien(), true);
	}

	private void createRedenenFotobesprekingVelden(ScreenitForm<MammaLezing> form, boolean alleenLezen)
	{
		var redenenFotobesprekingRadioloogSelector = new ScreenitListMultipleChoice<MammaLezingRedenenFotobesprekingRadioloog>(
			"redenenFotobesprekingRadioloog",
			Arrays.asList(MammaLezingRedenenFotobesprekingRadioloog.values()),
			new EnumChoiceRenderer<>());
		redenenFotobesprekingRadioloogSelector.setEnabled(!alleenLezen);
		form.add(redenenFotobesprekingRadioloogSelector);

		var redenenFotobesprekingMbberSelector = new ScreenitListMultipleChoice<MammaLezingRedenenFotobesprekingMbber>("redenenFotobesprekingMbber",
			Arrays.asList(MammaLezingRedenenFotobesprekingMbber.values()),
			new EnumChoiceRenderer<>());
		redenenFotobesprekingMbberSelector.setEnabled(!alleenLezen);
		form.add(redenenFotobesprekingMbberSelector);
	}

	private void dialoogOpslaanToevoegen(MammaBeoordelenHuidigeRondePanel beoordelingPanel)
	{
		beoordelenPreLezingOpslaanDialoog = new MammaBeoordelenPreLezingOpslaanDialoog(IDialog.CONTENT_ID, getModel(), beoordelingPanel)
		{
			@Override
			public void close(AjaxRequestTarget target)
			{
				beoordelingPanel.setOpslaanPopupGezien(true);
				beoordelingPanel.reloadMbberComponent(target);
				beoordelingPanel.reloadVisueleInspectiePanel(target);
				dialog.close(target);
			}
		};
	}

	private boolean heeftAfwijkingen()
	{
		return beoordelingPanel.getModelObject().getOnderzoek().getSignaleren().isHeeftAfwijkingen();
	}

	private boolean lezingNietOpgeslagen()
	{
		return MammaBeoordelingStatus.EERSTE_LEZING.equals(beoordelingPanel.getModelObject().getStatus()) ||
			MammaBeoordelingStatus.TWEEDE_LEZING.equals(beoordelingPanel.getModelObject().getStatus());
	}

	private MammaLezingParameters maakLezingParameters(MammaLezing lezing)
	{
		var verslagNogNietDefinitief = lezing.getBeoordeling() == null
			|| !MammaBeoordelingStatus.VERSLAG_MAKEN.equals(lezing.getBeoordeling().getStatus());
		var onderzoek = beoordelingPanel.getModelObject().getOnderzoek();
		return new MammaLezingParameters()
			.setInzien(!verslagNogNietDefinitief)
			.setAmputatie(onderzoek.getAmputatie())
			.setToonBiradsOpmerkingVeld(true)
			.setToonTomosyntheseSlicesRadioButtons(MammaOnderzoekType.TOMOSYNTHESE == onderzoek.getOnderzoekType());
	}

	private void createOpslaanCallbackAjaxBehavior()
	{
		opslaanCallback = new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				var paramValue = getComponent().getRequest().getRequestParameters().getParameterValue(MAMMA_BRIDGE_IMAGES_SEEN_RESPONSE_OBJECT_STRING).toString();
				try
				{
					var userSession = imsService.parseFhirMessage(paramValue);
					var reply = userSession.getContext().getLayoutImages().getRequestLayoutsImagesSeenCurrentFocus().getReply();
					if (checkIfClientEnAccessionNumberValid(userSession))
					{
						if ("true".equalsIgnoreCase(reply) || "false".equalsIgnoreCase(reply))
						{
							lezingOpslaan(target, "true".equalsIgnoreCase(reply));
						}
						else
						{
							LOG.warn("IMS geeft geen boolean voor all images seen request, response was {}", reply);
							error(imsService.handleError(reply, ScreenitSession.get().getIngelogdeOrganisatieMedewerker(), b -> getString((String) b),
								beoordelingPanel.getModelObject().getOnderzoek().getId()));
							beoordelingPanel.blokeerOpslaan(target);
						}
					}
					else
					{
						error(getString("error.mammobridge.onderzoeknietgelijk"));
						beoordelingPanel.blokeerOpslaan(target);
					}
				}
				catch (IOException e)
				{
					LOG.warn("Kon IMS fhir bericht niet parsen: {}", paramValue);
				}
			}
		};
		add(opslaanCallback);
	}

	private boolean checkIfClientEnAccessionNumberValid(FhirUserSession userSession)
	{
		var onderzoek = beoordelingPanel.getModelObject().getOnderzoek();
		var userSessionBsn = userSession.getFocus().getPatient().getIdentifier().getValue();
		var userSessionStudyId = userSession.getFocus().getImagingStudy().getAccession().getValue();
		var uitnodiging = onderzoek.getAfspraak().getUitnodiging();
		var client = uitnodiging.getBrief().getClient();
		var bsn = client.getPersoon().getBsn();
		var accessionNumber = uitnodiging.getScreeningRonde().getUitnodigingsNr().toString();
		var clientEnOnderzoekGelijk = bsn.equals(userSessionBsn) && accessionNumber.equals(userSessionStudyId);
		if (!clientEnOnderzoekGelijk)
		{
			var error = String.format(
				"IMS all images seen onderzoek of client is niet gelijk aan openstaande onderzoek of client in Screenit. IMS gaf client %s met onderzoek %s, ScreenIT heeft client met onderzoek %s open.",
				userSessionBsn, userSessionStudyId, accessionNumber);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_BE_IMS_HTTP_FOUT, ScreenitSession.get().getIngelogdeOrganisatieMedewerker(), client, error, Bevolkingsonderzoek.MAMMA);
		}
		return clientEnOnderzoekGelijk;
	}

	private void lezingOpslaan(AjaxRequestTarget target, boolean allImagesSeen)
	{
		var lezingModel = getModel();
		if ((heeftAfwijkingen() || !allImagesSeen) && lezingNietOpgeslagen())
		{
			prePopupBiradsWaardeLinks = getModelObject().getBiradsLinks();
			prePopupBiradsWaardeRechts = getModelObject().getBiradsRechts();
			beoordelenPreLezingOpslaanDialoog.setAllImagesSeen(allImagesSeen);
			beoordelenPreLezingOpslaanDialoog.setMbbSignalering(heeftAfwijkingen());
			beoordelenPreLezingOpslaanDialoog.updateTitle();
			dialog.openWith(target, beoordelenPreLezingOpslaanDialoog);
		}
		else
		{
			beoordelingPanel.lezingOpslaan(lezingModel, target, laesieDtos.getObject());
		}
	}

	public void blokeerOpslaan(AjaxRequestTarget target)
	{
		formSubmitBtn.setEnabled(false);
		target.add(formSubmitBtn);
	}
}
