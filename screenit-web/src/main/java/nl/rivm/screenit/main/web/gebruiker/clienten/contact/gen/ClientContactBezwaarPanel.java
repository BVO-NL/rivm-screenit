package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.bezwaar.edit.BezwaarEditPanel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class ClientContactBezwaarPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<BezwaarMoment> bezwaarMomentModel;

	private final IModel<Client> clientModel;

	private final IModel<List<FileUpload>> directFiles = new ListModel<>();

	private List<BezwaarGroupViewWrapper> directWrappers;

	private final IModel<List<FileUpload>> verwijderenFiles = new ListModel<>();

	private List<BezwaarGroupViewWrapper> verwijderenWrappers;

	private IModel<UploadDocument> document;

	private final IModel<Boolean> bezwaarAanvragenMetHandtekeningHerinnering = Model.of(false);

	public ClientContactBezwaarPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;
		bezwaarMomentModel = ModelUtil.cModel(new BezwaarMoment());
		var modelObject = bezwaarMomentModel.getObject();
		modelObject.setManier(ClientContactManier.FORMULIER_VOOR_AANVRAGEN);
		modelObject.setClient(clientModel.getObject());

		var clientContactManieren = new ArrayList<>(List.of(ClientContactManier.values()));
		if (!heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN, Recht.GEBRUIKER_BEZWAAR_BRP))
		{
			clientContactManieren.remove(ClientContactManier.GEGEVENS_VERWIJDEREN);
		}
		DropDownChoice<ClientContactManier> manier = new ScreenitDropdown<>("manier", new PropertyModel<>(bezwaarMomentModel, "manier"),
			new ListModel<>(clientContactManieren), new EnumChoiceRenderer<>(this));
		manier.setOutputMarkupPlaceholderTag(true);
		manier.setRequired(true);
		manier.setLabel(Model.of("Manier van afmelden"));
		add(manier);

		var bezwaarMakenContainer = new WebMarkupContainer("bezwaarMakenContainer");
		var directBezwaarMakenContainer = getDirectBezwaarMakenContainer();
		bezwaarMakenContainer.add(directBezwaarMakenContainer);
		var aanvraagFormulierBezwaarMakenContainer = getAanvraagFormulierBezwaarMakenContainer();
		bezwaarMakenContainer.add(aanvraagFormulierBezwaarMakenContainer);
		var gegevensVerwijderenContainer = getGegevensVerwijderenContainer();
		bezwaarMakenContainer.add(gegevensVerwijderenContainer);
		add(bezwaarMakenContainer);

		manier.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				aanvraagFormulierBezwaarMakenContainer.setVisible(!isDirectBezwaarOfVerwijderen());
				target.add(aanvraagFormulierBezwaarMakenContainer);
				directBezwaarMakenContainer.setVisible(isDirectBezwaar());
				target.add(directBezwaarMakenContainer);
				gegevensVerwijderenContainer.setVisible(isGegevensVerwijderen());
				target.add(gegevensVerwijderenContainer);
			}
		});
	}

	private List<BezwaarGroupViewWrapper> getHuidigeWrappers()
	{
		if (isDirectBezwaar())
		{
			return directWrappers;
		}
		return verwijderenWrappers;
	}

	private IModel<List<FileUpload>> getHuidigeFiles()
	{
		if (isDirectBezwaar())
		{
			return directFiles;
		}
		return verwijderenFiles;
	}

	private boolean isDirectBezwaar()
	{
		return ClientContactManier.DIRECT == bezwaarMomentModel.getObject().getManier();
	}

	private boolean isGegevensVerwijderen()
	{
		return ClientContactManier.GEGEVENS_VERWIJDEREN == bezwaarMomentModel.getObject().getManier();
	}

	private boolean isDirectBezwaarOfVerwijderen()
	{
		return isDirectBezwaar() || isGegevensVerwijderen();
	}

	private WebMarkupContainer getDirectBezwaarMakenContainer()
	{
		var container = new WebMarkupContainer("directBezwaarMakenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(false);

		var uploadField = new FileUploadField("directBezwaarUpload", directFiles);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setLabel(Model.of("Bestand"));
		container.add(uploadField);

		var laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();
		directWrappers = bezwaarService.getEditBezwaarGroupViewWrappers(clientModel.getObject(), laatsteVoltooideBezwaarMoment, true, BezwaarType.ALGEMENE_BEZWAAR_TYPES);
		container.add(new BezwaarEditPanel("bezwaarAanpassenPanel", directWrappers));

		return container;
	}

	private WebMarkupContainer getGegevensVerwijderenContainer()
	{
		var container = new WebMarkupContainer("gegevensVerwijderenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(false);

		var uploadField = new FileUploadField("gegevensVerwijderenUpload", verwijderenFiles);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setLabel(Model.of("Bestand"));
		container.add(uploadField);

		List<BezwaarType> bezwaarTypes = new ArrayList<>();
		if (heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN))
		{
			bezwaarTypes.add(BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER);
		}
		if (heeftRechtVoor(Recht.GEBRUIKER_BEZWAAR_BRP))
		{
			bezwaarTypes.add(BezwaarType.GEEN_OPNAME_UIT_BPR);
		}
		var laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();
		verwijderenWrappers = bezwaarService.getEditBezwaarGroupViewWrappers(clientModel.getObject(), laatsteVoltooideBezwaarMoment, true, bezwaarTypes);
		container.add(new BezwaarEditPanel("gegevensVerwijderenPanel", verwijderenWrappers));

		return container;
	}

	private WebMarkupContainer getAanvraagFormulierBezwaarMakenContainer()
	{
		var container = new WebMarkupContainer("bezwaarAanvragenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(true);

		var bezwaarAanvragenRadioChoice = new RadioChoice<>("bezwaarAanvragen", bezwaarAanvragenMetHandtekeningHerinnering,
			new ListModel<>(Arrays.asList(Boolean.FALSE, Boolean.TRUE)),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(Boolean object)
				{
					var label = getString(EnumStringUtil.getPropertyString(bezwaarMomentModel.getObject().getManier()));

					if (Boolean.TRUE.equals(object))
					{
						label += " – handtekening vergeten";
					}
					return label;
				}
			});
		bezwaarAanvragenRadioChoice.setPrefix("<label class=\"radio\">");
		bezwaarAanvragenRadioChoice.setSuffix("</label>");
		container.add(bezwaarAanvragenRadioChoice);
		return container;
	}

	@Override
	public void validate()
	{
		if (!isDirectBezwaarOfVerwijderen())
		{
			return;
		}

		if (getHuidigeFiles().getObject().size() != 1)
		{
			error(getString("error.een.bestand.uploaden.bezwaarformulier"));
		}
		if (!bezwarenGewijzigd())
		{
			error(getString("error.bezwaar.niet.gewijzigd"));
		}
	}

	private boolean bezwarenGewijzigd()
	{
		var laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();

		return bezwaarService.bezwarenGewijzigd(laatsteVoltooideBezwaarMoment, getHuidigeWrappers(), null);
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		var meldingen = super.getOpslaanMeldingen();
		if (isDirectBezwaarOfVerwijderen())
		{
			stelFileVeilig();
		}

		if (isGegevensVerwijderen())
		{
			meldingen.add(getString("gebruik.gegevens.waarschuwing"));
		}
		return meldingen;
	}

	private void stelFileVeilig()
	{
		try
		{
			if (getHuidigeFiles().getObject() != null)
			{
				var upload = getHuidigeFiles().getObject().get(0);
				var definitieFile = upload.writeToTempFile();

				var uploadDocument = new UploadDocument();
				uploadDocument.setActief(Boolean.TRUE);
				uploadDocument.setContentType(upload.getContentType());
				uploadDocument.setFile(definitieFile);
				uploadDocument.setNaam(upload.getClientFileName());
				document = ModelUtil.cModel(uploadDocument);
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij uploaden van een bezwaaruploaden naar tmp directory: ", e);
			error(getString("error.onbekend"));
		}
	}

	private boolean heeftRechtVoor(Recht... rechten)
	{
		var ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		return autorisatieService.getActieVoorMedewerker(ingelogdeGebruiker, null, rechten) != null;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		var objecten = super.getOpslaanObjecten();
		var wrappers = getHuidigeWrappers();
		objecten.put(ExtraOpslaanKey.BEZWAAR_WRAPPERS, wrappers);

		var bezwaarMoment = bezwaarMomentModel.getObject();
		if (isDirectBezwaarOfVerwijderen())
		{
			try
			{
				var client = bezwaarMoment.getClient();
				client.getBezwaarMomenten().size(); 
				var uploadDocument = document.getObject();
				uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.BEZWAAR, client.getId());

				if (moetBezwaarMomentAangemaaktWorden(wrappers))
				{
					bezwaarMoment.setBezwaarBrief(uploadDocument);
					bezwaarMoment.setStatus(AanvraagBriefStatus.BRIEF);
					bezwaarMoment.setStatusDatum(currentDateSupplier.getDate());
					hibernateService.saveOrUpdateAll(bezwaarMoment);

					objecten.put(ExtraOpslaanKey.BEZWAAR, ModelProxyHelper.deproxy(bezwaarMomentModel.getObject()));
					objecten.put(ExtraOpslaanKey.BEZWAAR_VRAGEN_OM_HANDTEKENING, bezwaarAanvragenMetHandtekeningHerinnering.getObject());
				}
				else if (moetOnderzoekresultatenActieAangemaaktWorden(wrappers))
				{
					var onderzoeksresultatenActie = new OnderzoeksresultatenActie();
					onderzoeksresultatenActie.setGetekendeBrief(uploadDocument);
					onderzoeksresultatenActie.setClient(client);

					objecten.put(ExtraOpslaanKey.ONDERZOEKSRESULTATEN_ACTIE, onderzoeksresultatenActie);
				}
				return objecten;
			}
			catch (IOException | IllegalStateException e)
			{
				LOG.error("Bezwaar kon niet per direct worden voltooid!");
			}
		}

		objecten.put(ExtraOpslaanKey.BEZWAAR, ModelProxyHelper.deproxy(bezwaarMomentModel.getObject()));
		objecten.put(ExtraOpslaanKey.BEZWAAR_VRAGEN_OM_HANDTEKENING, bezwaarAanvragenMetHandtekeningHerinnering.getObject());
		return objecten;
	}

	private boolean moetBezwaarMomentAangemaaktWorden(List<BezwaarGroupViewWrapper> wrappers)
	{
		return isDirectBezwaar() || wrappers.stream().flatMap(groupWrapper -> groupWrapper.getBezwaren().stream())
			.anyMatch(wrapper -> Boolean.TRUE.equals(wrapper.getActief()) && wrapper.getType() == BezwaarType.GEEN_OPNAME_UIT_BPR);
	}

	private boolean moetOnderzoekresultatenActieAangemaaktWorden(List<BezwaarGroupViewWrapper> wrappers)
	{
		return wrappers.stream().flatMap(groupWrapper -> groupWrapper.getBezwaren().stream())
			.anyMatch(wrapper -> Boolean.TRUE.equals(wrapper.getActief()) && wrapper.getType() == BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER);
	}
}
