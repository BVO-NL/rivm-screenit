package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

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

import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class CervixBetalingSepaUploadDocumentDownloadLinkPanel extends GenericPanel<UploadDocument>
{

	public CervixBetalingSepaUploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model)
	{
		super(id, model);
		add(new UploadDocumentLink("download", model, true)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(CervixBetalingSepaUploadDocumentDownloadLinkPanel.this.getDefaultModelObject() != null);
			}

			@Override
			public void onClick()
			{
				super.onClick();
				loggingBijOnClick();
			}

			@Override
			public boolean isEnabled()
			{
				return CervixBetalingSepaUploadDocumentDownloadLinkPanel.this.isEnabled();
			}
		});
	}

	protected abstract void loggingBijOnClick();
}
