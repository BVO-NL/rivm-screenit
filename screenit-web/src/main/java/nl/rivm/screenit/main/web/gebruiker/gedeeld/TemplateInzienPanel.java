package nl.rivm.screenit.main.web.gebruiker.gedeeld;

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
import java.time.Duration;
import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.util.GebeurtenisUtil;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.util.BriefUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.AjaxDownloadBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jspecify.annotations.NonNull;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

@Slf4j
public class TemplateInzienPanel extends GenericPanel<Brief>
{
	private final BootstrapDialog pdfDialog = new BootstrapDialog("pdfDialog");

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private BriefService briefService;

	public TemplateInzienPanel(String id, IModel<Brief> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		var datum = BriefUtil.geefDatumVoorGebeurtenisoverzicht(getModelObject());
		maakBriefInzienContent(getModelObject(), datum);
	}

	private void maakBriefInzienContent(Brief brief, Date datum)
	{
		add(pdfDialog);
		var inzienContainer = new WebMarkupContainer("inzienContainer");
		inzienContainer.setOutputMarkupId(true);

		var inzienGroep = new WebMarkupContainer("inzienGroep");

		Label inzienMsg;
		if (brief.getBriefDefinitie() != null)
		{
			inzienMsg = new Label("inzienMelding");
			inzienMsg.setVisible(false);
			var briefTemplate = brief.getBriefDefinitie().getDocument();
			inzienGroep.add(new IndicatingAjaxLink<Void>("inzien")
			{
				@Override
				public void onClick(AjaxRequestTarget ajaxRequestTarget)
				{
					try
					{
						var pdf = baseBriefService.maakPdfVanUploadDocument(briefTemplate);

						ajaxRequestTarget.appendJavaScript("$('.modal.fade.in').not('#sessieVerlopenDialog').addClass('previousDialog').modal('hide');");

						pdfDialog.openWith(ajaxRequestTarget, new PdfViewerPanel(IDialog.CONTENT_ID, pdf));
					}
					catch (Exception e)
					{
						LOG.error(e.getMessage());
					}
				}
			});
		}
		else
		{
			inzienMsg = new Label("inzienMelding", "Gebruikte template niet beschikbaar, want deze brief is " +
				(brief.getTemplateNaam() == null ? "nog niet verzonden" : "verzonden voordat ScreenIT de templates van de verzonden brieven ging bewaren, of nog niet verzonden"));
			inzienGroep.setVisible(false);
		}
		var downloadLink = getDownloadLink(brief);
		inzienGroep.add(downloadLink);
		var inzienMeldingGroup = new WebMarkupContainer("inzienMeldingGroup");
		inzienMeldingGroup.setOutputMarkupId(true);
		inzienMeldingGroup.add(inzienMsg);
		inzienMeldingGroup.setVisible(inzienMsg.isVisible());
		inzienContainer.add(inzienMeldingGroup);

		GebeurtenisUtil.voegBriefTypeOfNaamBriefToe(inzienGroep, brief);

		inzienGroep.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(datum), "dd-MM-yyyy"));
		inzienGroep.setOutputMarkupId(true);

		inzienContainer.add(inzienGroep);
		add(inzienContainer);
	}

	private @NonNull Component getDownloadLink(Brief brief)
	{
		var briefGuid = brief.getCommHubGuid();
		var downloadBehavior = createAjaxDownloadBehavior(briefGuid);

		return createIndicatingAjaxLink(briefGuid, downloadBehavior);
	}

	private @NonNull IndicatingAjaxLink<Void> createIndicatingAjaxLink(String briefGuid, AjaxDownloadBehavior downloadBehavior)
	{
		var link = new IndicatingAjaxLink<Void>("download")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				try
				{
					if (briefService.getVerstuurdeBrief(briefGuid).isPresent())
					{
						downloadBehavior.initiate(target);
					}
					else
					{
						error("Er is een fout bij het ophalen van de brief.");
					}
				}
				catch (Exception e)
				{
					LOG.error("Fout bij het ophalen van brief PDF bij CommHub", e);
					error("Er is een fout bij het ophalen van de brief.");
				}
			}
		};
		link.add(downloadBehavior);
		link.setVisible(StringUtils.isNotBlank(briefGuid));
		return link;
	}

	private @NonNull AjaxDownloadBehavior createAjaxDownloadBehavior(String briefGuid)
	{
		return new AjaxDownloadBehavior(new AbstractResource()
		{
			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				return getResourceResponse(new WriteCallback()
				{
					@Override
					public void writeData(Attributes attributes)
					{
						try
						{
							var verstuurdeBrief = briefService.getVerstuurdeBrief(briefGuid);
							if (verstuurdeBrief.isPresent())
							{
								attributes.getResponse().getOutputStream().write(verstuurdeBrief.get());
							}
						}
						catch (IOException e)
						{
							LOG.error("Fout bij schrijven brief naar outputstream voor GUID '{}'", briefGuid, e);
						}
					}
				});
			}
		});
	}

	private AbstractResource.ResourceResponse getResourceResponse(AbstractResource.WriteCallback callback)
	{
		var response = new AbstractResource.ResourceResponse();
		response.setContentType("application/pdf");
		response.setFileName("brief.pdf");
		response.setContentDisposition(ContentDisposition.ATTACHMENT);
		response.setCacheDuration(Duration.ZERO);
		response.setWriteCallback(callback);
		return response;
	}
}
