package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon;

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

import java.io.IOException;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.COLON_CLIENT_SR_FIT_ANALYSE_RESULTAAT_INZIEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonFitRegistratieInzienPanel extends AbstractGebeurtenisDetailPanel
{
	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private ColonBaseFitService fitService;

	private BootstrapDialog confirmDialog;

	private IModel<List<FileUpload>> file = new ListModel<>();

	private FileUploadField uploadField;

	private UploadDocument uploadDocument;

	private IndicatingAjaxSubmitLink formUploadBtn;

	private Form uploadForm;

	public ColonFitRegistratieInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		uploadForm = new Form<>("uploadForm", getModel());
		uploadForm.setOutputMarkupId(true);
		uploadForm.setOutputMarkupPlaceholderTag(true);
		add(uploadForm);

		uploadForm.add(DateLabel.forDatePattern("registratie.statusDatum", "dd-MM-yyyy"));
		uploadForm.add(DateLabel.forDatePattern("registratie.afnameDatum", "dd-MM-yyyy"));
		uploadForm.add(DateLabel.forDatePattern("registratie.analyseDatum", "dd-MM-yyyy"));
		uploadForm.add(new Label("registratie.fitLaboratorium.naam"));
		uploadForm.add(new EnumLabel<ColonFitType>("registratie.type"));

		var registratie = getModelObject().getRegistratie();
		String uitslag = null;
		ColonFitRegistratieStatus status = null;

		if (registratie != null)
		{
			status = registratie.getStatus();
			uitslag = fitService.getToonbareWaarde(registratie);
			if (uitslag != null)
			{
				uploadForm.add(new Label("uitslag", uitslag));
			}
		}
		if (uitslag == null)
		{
			uploadForm.add(new Label("uitslag", ""));
		}

		var interpretatie = ColonFitRegistratieUtil.getInterpretatie(registratie, status, true);
		uploadForm.add(new Label("interpretatie", interpretatie));
		if (status != null)
		{
			uploadForm.add(new EnumLabel<ColonFitRegistratieStatus>("registratie.status"));
		}
		else
		{
			uploadForm.add(new Label("registratie.status", ""));
		}
		uploadForm.add(new Label("registratie.barcode"));

		createUploadField();
		createVervangenUploadSubmitBtn();

		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);
	}

	private void createVervangenUploadSubmitBtn()
	{
		formUploadBtn = new IndicatingAjaxSubmitLink("uploadSubmitBtn")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (file.getObject().size() == 1)
				{
					var fileUpload = file.getObject().get(0);
					try
					{
						maakUploadDocument(fileUpload);
						var client = getModelObject().getRegistratie().getScreeningRonde().getDossier().getClient();
						uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.COLON_UITSLAG_VERWIJDEREN_CLIENT_BRIEF, client.getId());
						colonDossierService.vervangUitslagVerwijderenDocument(getModelObject().getRegistratie(), uploadDocument);
						setResponsePage(new ClientDossierPage(ModelUtil.sModel(client)));
					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een formulier: ", e);
						error(getString("error.onbekend"));
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als formulier");
					error(getString("error.onjuistaantalfiles"));
				}
			}
		};
		formUploadBtn.setOutputMarkupId(true);
		formUploadBtn.setOutputMarkupPlaceholderTag(true);
		formUploadBtn.setVisible(false);
		uploadForm.add(formUploadBtn);
	}

	private void createUploadField()
	{
		uploadField = new FileUploadField("fileUpload", file);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setOutputMarkupId(true);
		uploadField.setOutputMarkupPlaceholderTag(true);
		uploadField.setVisible(magVerwijderen());
		uploadForm.add(uploadField);
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		ConfirmingIndicatingAjaxSubmitLink<Void> button = new ConfirmingIndicatingAjaxSubmitLink<>(id, uploadForm, confirmDialog, "label.ifobtuitslag.verwijderen")
		{
			@Override
			protected boolean skipConfirmation()
			{
				if (file.getObject().size() == 1)
				{
					var fileUpload = file.getObject().get(0);
					try
					{
						maakUploadDocument(fileUpload);
					}
					catch (Exception e)
					{
						LOG.error("Fout bij uploaden van een formulier: ", e);
						error(getString("error.onbekend"));
						return true;
					}
				}
				else
				{
					LOG.error("Er mag maar 1 bestand geuploaded worden als formulier");
					error(getString("error.onjuistaantalfiles"));
					return true;
				}
				return super.skipConfirmation();
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var buis = ColonFitRegistratieInzienPanel.this.getModelObject().getRegistratie();
				var uitnodiging = (ColonUitnodiging) ColonFitRegistratieInzienPanel.this.getModelObject().getUitnodiging();
				try
				{
					uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.COLON_UITSLAG_VERWIJDEREN_CLIENT_BRIEF,
						uitnodiging.getScreeningRonde().getDossier().getClient().getId());
				}
				catch (IOException e)
				{
					LOG.error("Fout bij uploaden van een bezwaar formulier: ", e);
					error(getString("error.onbekend"));
				}
				colonDossierService.verwijderFitAnalyseResultaat(buis, uploadDocument, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
				ScreenitSession.get().info(ColonFitRegistratieInzienPanel.this.getString("resultaat.verwijderd"));

				setResponsePage(new ClientDossierPage(ModelUtil.sModel(uitnodiging.getScreeningRonde().getDossier().getClient())));
			}
		};
		button.add(new Label("label", getString("label.verwijderen")));
		button.add(new AttributeAppender("class", Model.of(" btn-danger")));
		button.setVisible(magVerwijderen());
		parent.add(button);
	}

	private boolean magVerwijderen()
	{
		var magVerwijderen = ScreenitSession.get().checkPermission(Recht.COLON_CLIENT_SR_FIT_ANALYSE_RESULTAAT_INZIEN, Actie.VERWIJDEREN);
		var screeningRondeGebeurtenis = getModelObject();
		var uitnodiging = (ColonUitnodiging) screeningRondeGebeurtenis.getUitnodiging();
		if (uitnodiging != null)
		{
			var dossier = uitnodiging.getScreeningRonde().getDossier();
			var registratie = screeningRondeGebeurtenis.getRegistratie();
			if (!ColonFitRegistratieUtil.isLaatsteUitslagVanLaatsteRonde(dossier, uitnodiging.getGekoppeldeFitRegistratie().getStatusDatum()))
			{
				magVerwijderen = false;
			}
			else if (dossier.getLaatsteScreeningRonde().getLaatsteAfspraak() != null && dossier.getLaatsteScreeningRonde().getLaatsteAfspraak().getConclusie() != null)
			{

				magVerwijderen = false;
			}
			else if (registratie != null)
			{
				var status = registratie.getStatus();
				if (ColonFitRegistratieStatus.VERWIJDERD.equals(status) || registratie.getType().equals(ColonFitType.GOLD) && registratie.getUitslag() == null)
				{

					magVerwijderen = false;
				}
			}
		}
		return magVerwijderen;
	}

	private void maakUploadDocument(FileUpload fileUpload) throws Exception
	{
		uploadDocument = ScreenitSession.get().fileUploadToUploadDocument(fileUpload);
	}

	@Override
	protected void addDocumentVervangenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		IndicatingAjaxLink<Void> btn = new IndicatingAjaxLink<>(id)
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uploadField.setVisible(true);
				formUploadBtn.setVisible(ColonFitRegistratieStatus.VERWIJDERD.equals(ColonFitRegistratieInzienPanel.this.getModelObject().getRegistratie().getStatus()));
				target.add(uploadForm);
			}
		};
		btn.setVisible(ColonFitRegistratieStatus.VERWIJDERD.equals(getModelObject().getRegistratie().getStatus()));
		parent.add(btn);
	}

	@Override
	protected void addDocumentDownloadenButton(String id, GebeurtenisPopupBasePanel parent)
	{
		var briefDownloadBtn = new UploadDocumentLink(id, new PropertyModel<>(getModel(), "registratie.verwijderbrief"), true);
		briefDownloadBtn.setVisible(ColonFitRegistratieStatus.VERWIJDERD.equals(getModelObject().getRegistratie().getStatus()));
		parent.add(briefDownloadBtn);
	}
}
