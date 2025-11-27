package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.cytologie.CervixCytologieVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen.ColonMdlVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen.ColonPaVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie.MammaFollowUpPathologieVerslagInzienPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientVerslagPage extends ClientPage
{

	@SpringBean
	private VerslagService verslagService;

	@SpringBean
	private VerwerkVerslagService verwerkVerslagService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private BaseVerslagService baseVerslagService;

	private final IModel<? extends Verslag<?, ?>> verslagModel;

	private final BootstrapDialog verwijderDialog;

	public ClientVerslagPage(IModel<? extends Verslag<?, ?>> verslagModel)
	{
		super(new PropertyModel<>(verslagModel, "screeningRonde.dossier.client"));
		this.verslagModel = verslagModel;

		var verslag = verslagModel.getObject();
		var isNieuw = verslag.getId() == null;
		var client = verslag.getScreeningRonde().getDossier().getClient();
		var magVerslagDownloaden = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_VERSLAGEN, Actie.AANPASSEN, client);
		boolean magVerslagVerwijderen;

		switch (verslag.getType())
		{
		case MDL:
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.VERWIJDEREN, client);
			break;
		case PA_LAB:
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.VERWIJDEREN, client);
			break;
		case CERVIX_CYTOLOGIE:
			magVerslagVerwijderen = false;
			magVerslagDownloaden = false;
			break;
		case MAMMA_PA_FOLLOW_UP:
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.VERWIJDEREN, client);
			magVerslagDownloaden = false;
			break;
		default:
			throw new IllegalStateException("Unexpected value: " + verslag.getType());
		}

		verwijderDialog = new BootstrapDialog("dialog");
		add(verwijderDialog);
		addInzienPanel(verslag);
		add(new ClientPaspoortPanel("passpoort", (IModel<Client>) getDefaultModel()));
		addVerslagVerwijderenButton(isNieuw, magVerslagVerwijderen);
		addDownloadButton(verslag, magVerslagDownloaden);
		addAnnulerenButton();
		addTerugButton();

		add(new WebMarkupContainer("isnieuw").setVisible(isNieuw));

		switch (verslag.getType())
		{
		case MDL:
			add(new Label("type", "MDL"));
			break;
		case PA_LAB:
			add(new Label("type", "PA"));
			break;
		case CERVIX_CYTOLOGIE:
			add(new Label("type", "Cytologie"));
			break;
		case MAMMA_PA_FOLLOW_UP:
			add(new Label("type", "Follow Up"));
			break;
		default:
			throw new IllegalStateException("Unexpected value: " + verslag.getType());
		}
	}

	private void addDownloadButton(Verslag<?, ?> verslag, boolean magVerslagDownloaden)
	{
		add(new ResourceLink<>("downloaden", new AbstractResource()
		{

			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse response = new ResourceResponse();
				response.setFileName("bericht.xml");
				response.setContentType("application/xml");
				response.getHeaders().addHeader("Cache-Control", "no-cache");
				response.setContentDisposition(ContentDisposition.ATTACHMENT);

				response.setWriteCallback(new WriteCallback()
										  {
											  @Override
											  public void writeData(Attributes attributes)
											  {
												  var cdaBericht = verslagModel.getObject().getOntvangenBericht();
												  var outputStream = attributes.getResponse().getOutputStream();

												  baseVerslagService.getBerichtXml(cdaBericht, outputStream);
											  }
										  }

				);
				return response;
			}
		}).setVisible(verslag.getOntvangenBericht() != null && magVerslagDownloaden));
	}

	@Override
	protected String getExtraTimeoutInfo()
	{
		return "Het verslag zal worden opgeslagen mits gegevens geldig zijn.";
	}

	private void addTerugButton()
	{
		add(new Link<>("terug")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
			}

		});
	}

	private void addAnnulerenButton()
	{
		add(new Link<>("annuleren")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
			}

		});
	}

	private void addVerslagVerwijderenButton(boolean isNieuw, boolean magVerslagVerwijderen)
	{
		IndicatingAjaxLink<Void> verwijderen = new ConfirmingIndicatingAjaxLink<>("verwijderen", verwijderDialog, "verwijder.verslag")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				var verslag = verslagModel.getObject();
				if (verslag.getType().getBevolkingsonderzoek() != Bevolkingsonderzoek.CERVIX)
				{
					ScreenitSession.get().info("Verslag verwijderd");
					baseVerslagService.verwijderVerslag(verslag, getIngelogdeOrganisatieMedewerker(), true);
					markeerFormulierenOpgeslagen(target);

					setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
				}
				else
				{
					ScreenitSession.get().warn("In BMHK is het niet toegestaan om een verslag te verwijderen");
				}
			}

		};
		add(verwijderen);
		verwijderen.setVisible(magVerslagVerwijderen && !isNieuw);
	}

	private void addInzienPanel(Verslag<?, ?> verslag)
	{
		switch (verslag.getType())
		{
		case MAMMA_PA_FOLLOW_UP:
			add(new MammaFollowUpPathologieVerslagInzienPanel("inzienPanel", (IModel<MammaFollowUpVerslag>) verslagModel));
			break;
		case CERVIX_CYTOLOGIE:
			add(new CervixCytologieVerslagInzienPanel("inzienPanel", (IModel<CervixCytologieVerslag>) verslagModel));
			break;
		case MDL:
			add(new ColonMdlVerslagInzienPanel("inzienPanel", (IModel<MdlVerslag>) verslagModel));
			break;
		case PA_LAB:
			add(new ColonPaVerslagInzienPanel("inzienPanel", (IModel<PaVerslag>) verslagModel));
			break;
		}
	}

	@Override
	protected Class<? extends MedewerkerBasePage> getActiveSubMenuClass()
	{
		return ClientVerslagenPage.class;
	}

	@Override
	protected Class<? extends MedewerkerBasePage> getActiveContextMenuClass()
	{
		return ClientVerslagenPage.class;
	}

	@Override
	public void detachModels()
	{
		super.detachModels();
		ModelUtil.nullSafeDetach(verslagModel);
	}

}
