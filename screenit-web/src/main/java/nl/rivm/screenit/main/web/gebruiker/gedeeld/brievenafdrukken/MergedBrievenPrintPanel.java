package nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.MergedBrievenRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.documentupload.wicket.UploadDocumentPdfObjectContainer;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MergedBrievenPrintPanel<MB extends MergedBrieven<?>> extends GenericPanel<MB>
{

	@SpringBean(name = "testModus")
	private Boolean testModus;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private MergedBrievenRepository mergedBrievenRepository;

	@SpringBean
	private CervixHuisartsService cervixHuisartsService;

	@SpringBean
	private LogService logService;

	public MergedBrievenPrintPanel(String id, IModel<MB> model)
	{
		super(id, model);

		var mergedBrievenModel = new PropertyModel<UploadDocument>(model, "mergedBrieven");

		var pdfObjectContainer = new UploadDocumentPdfObjectContainer("pdfObject", mergedBrievenModel);
		add(pdfObjectContainer);
		var toonTestUiElementen = preferenceService.getBoolean(PreferenceKey.TOON_TEST_ELEMENTEN.name(), false);
		add(new UploadDocumentLink("downloadPdf", mergedBrievenModel, true).setVisible(testModus && toonTestUiElementen));
		add(new AjaxLink<>("afdrukken")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				var mergedBrieven = model.getObject();
				if (mergedBrieven.getGeprint().equals(Boolean.FALSE))
				{
					mergedBrieven.setGeprint(true);
					mergedBrieven.setPrintDatum(currentDateSupplier.getDate());
					mergedBrieven.setAfgedruktDoor(ScreenitSession.get().getIngelogdeOrganisatieMedewerker().getMedewerker());
					if (HibernateHelper.deproxy(mergedBrieven) instanceof CervixRegioMergedBrieven)
					{
						cervixHuisartsService.updateLabformulierAanvraag((CervixRegioMergedBrieven) mergedBrieven);
					}
				}
				mergedBrievenRepository.save(model.getObject());
				logAction(mergedBrieven);
				sluiten(target);
			}
		}.setVisible(model.getObject().getGeprint().equals(Boolean.FALSE)));
	}

	private void logAction(MergedBrieven mergedBrieven)
	{
		var melding = "Document afgedrukt met type: ";
		if (mergedBrieven.getBriefType() == null)
		{
			melding += "ProjectBrief";
		}
		else
		{
			melding += EnumStringUtil.getPropertyString(mergedBrieven.getBriefType());
		}
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFGEDRUKT, ScreenitSession.get().getIngelogdAccount(), melding,
			mergedBrieven.getBriefType() != null ? mergedBrieven.getBriefType().getOnderzoeken() : null);
	}

	protected abstract void sluiten(AjaxRequestTarget target);

}
