package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

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

import java.io.File;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.BriefTypeChoiceRenderer;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.event.Broadcast;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;

import com.aspose.words.Document;

@Slf4j
public abstract class BaseDocumentTemplateTestenPage extends AlgemeenPage
{

	private enum TemplateBron
	{
		UPLOADED,
		AD_HOC
	}

	private static final int MIN_AANTAL_PARAGON = 1;

	private static final int MAX_AANTAL_PARAGON = 100;

	private IModel<ScreeningOrganisatie> selectedRegio;

	private final IModel<BriefType> selectedType;

	private final TemplateBron bron;

	protected final IModel<MergeField> mergeFieldModel = Model.of();

	protected final IModel<Boolean> zonderHandtekeningModel = Model.of(false);

	private final IModel<String> codeAddendumModel = Model.of("");

	private final IModel<Integer> aantalModel = Model.of(MIN_AANTAL_PARAGON);

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	protected AsposeService asposeService;

	@SpringBean
	private BaseBriefService briefService;

	@SpringBean
	private UitnodigingsDao uitnodigingsDao;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MessageService messageService;

	@SpringBean(name = "testModus")
	private Boolean testModus;

	private final IModel<List<FileUpload>> fileUploads = new ListModel<>();

	private final boolean automatischAfdrukkenParagonActief;

	private final DocumentTemplateTestenFieldsPanel fieldsContainer;

	private final IModel<DocumentTemplateTestWrapper> wrapperModel = Model.of(new DocumentTemplateTestWrapper());

	public BaseDocumentTemplateTestenPage()
	{
		MergeField.resetDefaultMergeFields();

		var wrapper = wrapperModel.getObject();
		var uitnodiging = wrapper.getCervixUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		var actieveIntakelocaties = organisatieService.getActieveIntakelocaties();
		if (CollectionUtils.isNotEmpty(actieveIntakelocaties))
		{
			wrapper.cloneIntakeLocatie(actieveIntakelocaties.getFirst());
		}

		var level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.MEDEWERKER_BEHEER_DOCUMENTENTEMPLATES);

		var screeningOrganisatieLijst = organisatieService.getAllActiefScreeningOrganisaties();
		selectedType = Model.of();
		selectedRegio = ModelUtil.sModel(screeningOrganisatieLijst.getFirst());

		if (ToegangLevel.REGIO.equals(level))
		{
			screeningOrganisatieLijst.clear();
			var so = ScreenitSession.get().getScreeningOrganisatie();
			if (so != null)
			{
				screeningOrganisatieLijst.add(so);
				selectedRegio = ModelUtil.sModel(so);
			}
		}

		automatischAfdrukkenParagonActief = briefService.isAutomatischAfdrukkenParagonActief() && testModus;

		bron = null;

		var form = new Form<Void>("regioForm");
		form.setMultiPart(true);
		add(form);

		var screeningOrganisatieDropdown = new ScreenitDropdown<ScreeningOrganisatie>("regio", selectedRegio,
			ModelUtil.listRModel(screeningOrganisatieLijst), new ChoiceRenderer<>("naam"));
		screeningOrganisatieDropdown.setNullValid(false);
		screeningOrganisatieDropdown.setRequired(true);
		form.add(screeningOrganisatieDropdown);
		screeningOrganisatieDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				send(fieldsContainer, Broadcast.BREADTH, target);
			}

		});

		var typeList = new ScreenitDropdown<>("brieven", new PropertyModel<>(this, "selectedType"),
			(IModel<List<BriefType>>) this::getVisibleBriefTypes, new BriefTypeChoiceRenderer());
		typeList.setOutputMarkupId(true);
		form.add(typeList);
		form.add(getPrintButton());
		form.add(getNaarParagonButton());
		form.add(getAlleNaarParagonButton());

		var paragonInstellingenContainer = new WebMarkupContainer("paragonInstellingenContainer");
		paragonInstellingenContainer.setVisible(automatischAfdrukkenParagonActief);
		form.add(paragonInstellingenContainer);

		var bron = new RadioChoice<>("bron", new PropertyModel<>(this, "bron"), Arrays.asList(TemplateBron.values()),
			new EnumChoiceRenderer<>(this));
		bron.setPrefix("<label class=\"radio\">");
		bron.setSuffix("</label>");
		bron.setRequired(true);
		bron.setOutputMarkupId(true);
		var uploadField = new FileUploadField("upload", fileUploads);
		uploadField.setOutputMarkupId(true);
		uploadField.setOutputMarkupPlaceholderTag(true);
		uploadField.setVisible(false);
		uploadField.add(new FileValidator(FileType.WORD_NIEUW));
		form.add(uploadField);
		form.add(bron);
		bron.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (BaseDocumentTemplateTestenPage.this.bron == TemplateBron.AD_HOC)
				{
					uploadField.setVisible(true);
					target.add(uploadField);
				}
				else
				{
					uploadField.setVisible(false);
					target.add(uploadField);
				}
			}
		});

		addAdditionalFormComponents(form);

		fieldsContainer = new DocumentTemplateTestenFieldsPanel("fieldsContainer", wrapperModel)
		{
			@Override
			protected List<MergeFieldTestType> getMergeTypes()
			{
				return BaseDocumentTemplateTestenPage.this.getMergeTypes();
			}

		};
		form.add(fieldsContainer);
		addMergFieldSelector(form, paragonInstellingenContainer);
	}

	private void setEmptyStringForNullValue(MergeField mergeField)
	{
		if (mergeField.getCurrentValue() == null)
		{
			mergeField.setCurrentValue("");
		}
	}

	private void addMergFieldSelector(Form<?> form, WebMarkupContainer paragonInstellingenContainer)
	{
		var mergeFieldForm = new ScreenitForm<>("mergeFieldForm");
		form.add(mergeFieldForm);
		var mergeFieldDropdown = new ScreenitDropdown<>("mergeField", mergeFieldModel, List.of(MergeField.values()), new ChoiceRenderer<>("fieldName"));
		mergeFieldDropdown.setNullValid(true);
		mergeFieldDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				BaseDocumentTemplateTestenPage.this.mergeFieldChanged(target);
				target.add(form);
			}
		});
		mergeFieldForm.add(mergeFieldDropdown);

		paragonInstellingenContainer.add(new TextField<>("codeAddendum", codeAddendumModel));

		var aantalNaarParagonField = new TextField<>("aantal", aantalModel, Integer.class);
		aantalNaarParagonField.setRequired(true);
		aantalNaarParagonField.add(RangeValidator.range(MIN_AANTAL_PARAGON, MAX_AANTAL_PARAGON));
		paragonInstellingenContainer.add(aantalNaarParagonField);
	}

	protected List<BriefType> zichtbareBriefTypesMetMergeField(List<BriefType> visibleBriefTypes, MergeField mergeField)
	{
		var briefTypes = new ArrayList<BriefType>();
		for (var briefType : visibleBriefTypes)
		{
			if (briefType.isActief())
			{
				var nieuwsteBriefDefinitie = briefService.getNieuwsteBriefDefinitie(briefType);
				if (nieuwsteBriefDefinitie != null)
				{
					var briefTemplate = uploadDocumentService.load(nieuwsteBriefDefinitie.getDocument());
					if (asposeService.heeftMergeField(briefTemplate, mergeField))
					{
						briefTypes.add(briefType);
					}
				}
			}
		}
		return briefTypes;
	}

	protected void mergeFieldChanged(AjaxRequestTarget target)
	{

	}

	protected abstract void addAdditionalFormComponents(Form<Void> form);

	protected abstract List<MergeFieldTestType> getMergeTypes();

	protected abstract List<BriefType> getVisibleBriefTypes();

	protected abstract List<Bevolkingsonderzoek> getBevolkingsonderzoeken();

	protected abstract Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception;

	private IndicatingAjaxSubmitLink getPrintButton()
	{
		return new IndicatingAjaxSubmitLink("printen")
		{

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					var mergedDocument = maakMergedDocumentVoorBriefType(getSelectedType());
					if (mergedDocument != null)
					{
						fieldsContainer.createAndShowPDF(target, mergedDocument);
					}
				}
				catch (Exception e)
				{
					LOG.error("Error converting doc to pdf", e);
					error("Er is iets misgegaan met het genereren van de pdf.");
				}
			}
		};
	}

	private IndicatingAjaxSubmitLink getNaarParagonButton()
	{
		var naarParagonButton = new IndicatingAjaxSubmitLink("naarParagon")
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					queuePdfVoorBriefType(getSelectedType());
				}
				catch (Exception e)
				{
					LOG.error("Error queueing pdf", e);
					error("Er is iets misgegaan met het aanbieden aan de queue.");
				}
			}
		};
		naarParagonButton.setVisible(automatischAfdrukkenParagonActief);
		return naarParagonButton;
	}

	private IndicatingAjaxSubmitLink getAlleNaarParagonButton()
	{
		var alleNaarParagonButton = new IndicatingAjaxSubmitLink("alleNaarParagon")
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				var beschikbareBriefTypes = new ArrayList<>(getVisibleBriefTypes());
				if (beschikbareBriefTypes.isEmpty())
				{
					error("Er zijn geen beschikbare templates.");
					return;
				}

				var origineelGeselecteerdType = getSelectedType();
				var aantalGeslaagd = 0;
				for (var briefType : beschikbareBriefTypes)
				{
					setSelectedType(briefType);
					try
					{
						queuePdfVoorBriefType(briefType);
						aantalGeslaagd++;
					}
					catch (Exception e)
					{
						LOG.error("Error queueing pdf voor briefType {}", briefType, e);
					}
				}
				setSelectedType(origineelGeselecteerdType);

				if (aantalGeslaagd == 0)
				{
					error("Geen enkele template is succesvol aan de queue aangeboden.");
				}
				else
				{
					info(aantalGeslaagd + " template(s) zijn aan de queue aangeboden.");
				}
			}
		};
		alleNaarParagonButton.setVisible(automatischAfdrukkenParagonActief);
		return alleNaarParagonButton;
	}

	private void queuePdfVoorBriefType(BriefType briefType) throws Exception
	{
		var mergedDocument = maakMergedDocumentVoorBriefType(briefType);
		if (mergedDocument == null)
		{
			return;
		}

		var context = bepaalBatchContext(briefType);
		var aantalNaarParagon = getAantalNaarParagon();
		for (var i = 0; i < aantalNaarParagon; i++)
		{
			var pdfBestand = briefService.genereerPdf(mergedDocument, "test_template_brieven", false);
			var bestandsNaam = UUID.randomUUID().toString();
			briefService.pdfBestandOpslaanVoorVersturen(pdfBestand, bestandsNaam);
			var dto = maakBriefafdrukopdrachtDto(briefType, bestandsNaam);
			messageService.queueMessage(MessageType.BRIEF_AFDRUKKEN, dto, context.name());
		}
		info("Template is " + aantalNaarParagon + " keer aan de queue aangeboden.");
	}

	private BriefafdrukopdrachtDto maakBriefafdrukopdrachtDto(BriefType briefType, String bestandsNaam)
	{
		var contextBrief = DocumentTemplateTestenFieldsPanel.createMailMergeContext(wrapperModel.getObject(), selectedRegio.getObject(), briefType,
			zonderHandtekeningModel.getObject()).getBrief();
		var geselecteerdeCodeAddendum = codeAddendumModel.getObject();
		var now = dateSupplier.getLocalDateTime();

		return BriefafdrukopdrachtDto.builder()
			.code(briefType.getBriefCode())
			.kenmerk("K1234567890ABCDEF")
			.timestamp(now.format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDDHHMMSS)))
			.codeAddendum(geselecteerdeCodeAddendum == null ? "" : geselecteerdeCodeAddendum.trim())
			.entityId(contextBrief == null ? null : contextBrief.getId())
			.entityType(contextBrief == null ? null : contextBrief.getClass())
			.resources(List.of(BriefafdrukopdrachtDto.Resource.builder().order(1).path(bestandsNaam).build()))
			.build();
	}

	private Integer getAantalNaarParagon()
	{
		return aantalModel.getObject();
	}

	private BatchApplicationType bepaalBatchContext(BriefType briefType)
	{
		if (briefType == null || briefType.getOnderzoeken() == null || briefType.getOnderzoeken().length != 1)
		{
			return BatchApplicationType.GENERALIS;
		}
		return BatchApplicationType.valueOf(briefType.getOnderzoeken()[0].name());
	}

	private Document maakMergedDocumentVoorBriefType(BriefType printType) throws Exception
	{
		if (printType == null)
		{
			error("Er is geen template gekozen.");
			return null;
		}

		var wrapper = wrapperModel.getObject();
		var bevolkingsonderzoeken = getBevolkingsonderzoeken();
		logService.logGebeurtenis(LogGebeurtenis.TESTEN_VAN_BRIEVEN, ScreenitSession.get().getIngelogdAccount(),
			bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[bevolkingsonderzoeken.size()]));

		var screeningOrganisatie = selectedRegio.getObject();
		var context = DocumentTemplateTestenFieldsPanel.createMailMergeContext(wrapper, screeningOrganisatie, printType, zonderHandtekeningModel.getObject());

		var bmhkLaboratorium = wrapper.getBmhkLaboratorium();
		var client = wrapper.getClient();
		var gbaGemeente = client.getPersoon().getGbaAdres().getGbaGemeente();
		var zasRetouradres = bmhkLaboratorium.getRetouradressen().getFirst();
		var overeenkomst = wrapper.getOvereenkomst();
		var cervixUitnodiging = wrapper.getCervixUitnodiging();

		try
		{
			var briefTemplate = bepaalBriefTemplate(printType);
			if (briefTemplate == null)
			{
				error("Er is geen template bestand opgegeven.");
				return null;
			}

			Document mergedDocument = null;

			var laatsteBeoordelingMetUitslag = MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(wrapper.getClient());
			var handmatigeRadioloog1 = laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().getMedewerker();
			var handmatigeRadioloog2 = laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().getMedewerker();
			if (!wrapper.isFreeTextBKRADIOLOOG())
			{
				laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().setMedewerker(wrapper.getRadioloog1());
				laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().setMedewerker(wrapper.getRadioloog2());
			}

			if (!wrapper.isFromDBINTAKELOCATIE())
			{
				setEmptyStringForNullValue(MergeField.IL_LOKATIE);
				setEmptyStringForNullValue(MergeField.IL_DIGITALE_INTAKE);
			}
			if (wrapper.isFromDBINTAKELOCATIE())
			{
				MergeField.IL_LOKATIE.setCurrentValue(null);
				MergeField.IL_DIGITALE_INTAKE.setCurrentValue(null);

				var intakeLocaties = organisatieService.getActieveIntakelocatiesBinnenRegio(screeningOrganisatie);
				var kamer = wrapper.getIntakeAfspraak().getKamer();
				var handmatigeIntakeLocatie = kamer.getIntakelocatie();
				for (var intakeLocatie : intakeLocaties)
				{
					kamer.setIntakelocatie(intakeLocatie);
					var document = proccesDocument(context, briefTemplate);
					mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
				}
				if (intakeLocaties.isEmpty())
				{
					error("Geen intakelocaties die vallen in de regio " + screeningOrganisatie.getNaam());
				}
				kamer.setIntakelocatie(handmatigeIntakeLocatie);
			}
			else if (wrapper.isFromDBBMHKLAB())
			{
				var labs = organisatieService.getActieveOrganisaties(BMHKLaboratorium.class);
				for (var lab : labs)
				{
					cervixUitnodiging.getMonster().setLaboratorium(lab);
					gbaGemeente.setBmhkLaboratorium(lab);
					context.setBmhkLaboratorium(lab);
					var document = proccesDocument(context, briefTemplate);
					mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
				}
				if (labs.isEmpty())
				{
					error("Geen BMHK laboratorium");
				}
				cervixUitnodiging.getMonster().setLaboratorium(bmhkLaboratorium);
				gbaGemeente.setBmhkLaboratorium(bmhkLaboratorium);
				context.setBmhkLaboratorium(bmhkLaboratorium);
			}
			else if (wrapper.isFromDBBKSTANDPLAATS())
			{
				var standplaatsen = standplaatsService.getActieveStandplaatsen(screeningOrganisatie);
				var standplaatsRonde = context.getClient().getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak()
					.getStandplaatsPeriode().getStandplaatsRonde();
				var handmatigeStandplaats = standplaatsRonde.getStandplaats();
				for (var standplaats : standplaatsen)
				{
					standplaatsRonde.setStandplaats(standplaats);
					var document = proccesDocument(context, briefTemplate);
					mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
				}
				if (standplaatsen.isEmpty())
				{
					error("Geen standplaatsen die vallen in de regio " + screeningOrganisatie.getNaam());
				}
				standplaatsRonde.setStandplaats(handmatigeStandplaats);
			}
			else
			{
				var document = proccesDocument(context, briefTemplate);
				mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
			}

			if (!wrapper.isFreeTextBKRADIOLOOG())
			{
				laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().setMedewerker(handmatigeRadioloog1);
				laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().setMedewerker(handmatigeRadioloog2);
			}

			return mergedDocument;
		}
		finally
		{
			gbaGemeente.setScreeningOrganisatie(null);
			zasRetouradres.setRegio(null);
			overeenkomst.setScreeningOrganisatie(null);
		}
	}

	private File bepaalBriefTemplate(BriefType briefType) throws Exception
	{
		if (bron == TemplateBron.UPLOADED)
		{
			var definitie = briefService.getNieuwsteBriefDefinitie(briefType);
			return definitie == null ? null : uploadDocumentService.load(definitie.getDocument());
		}
		if (CollectionUtils.isNotEmpty(fileUploads.getObject()))
		{
			return fileUploads.getObject().getFirst().writeToTempFile();
		}
		return null;
	}

	public BriefType getSelectedType()
	{
		return ModelUtil.nullSafeGet(selectedType);
	}

	public void setSelectedType(BriefType selectedType)
	{
		this.selectedType.setObject(selectedType);
	}

	public ScreeningOrganisatie getSelectedRegio()
	{
		return ModelUtil.nullSafeGet(selectedRegio);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedRegio);
		ModelUtil.nullSafeDetach(wrapperModel);
	}

	@Override
	protected List<MedewerkerMenuItem> getContextMenuItems()
	{
		var contextMenuItems = new ArrayList<MedewerkerMenuItem>();
		contextMenuItems.add(new MedewerkerMenuItem("label.documententemplates.overige", DocumentTemplateTestenPage.class));
		contextMenuItems.add(new MedewerkerMenuItem("label.documententemplates.bezwaar", BezwaarDocumentenTemplatesPage.class));
		return contextMenuItems;
	}
}
