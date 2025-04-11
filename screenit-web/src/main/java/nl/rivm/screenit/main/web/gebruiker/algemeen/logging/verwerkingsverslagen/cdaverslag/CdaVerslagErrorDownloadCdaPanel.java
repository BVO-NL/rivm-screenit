package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cdaverslag;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.service.BaseVerslagService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.springframework.beans.factory.annotation.Autowired;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

@Slf4j
public class CdaVerslagErrorDownloadCdaPanel extends GenericPanel<BerichtOntvangenLogEvent>
{

	@Autowired
	private BaseVerslagService verslagService;

	public CdaVerslagErrorDownloadCdaPanel(String id, IModel<BerichtOntvangenLogEvent> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("logRegel.gebeurtenisDatum", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("melding"));
		add(new ResourceLink<>("download", new AbstractResource()
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
												  var cdaBericht = CdaVerslagErrorDownloadCdaPanel.this.getModelObject().getBericht();
												  var outputStream = attributes.getResponse().getOutputStream();

												  verslagService.getBerichtXml(cdaBericht, outputStream);
											  }
										  }

				);
				return response;
			}
		}
		)).setVisible(getModelObject().getBericht() != null);
	}
}
