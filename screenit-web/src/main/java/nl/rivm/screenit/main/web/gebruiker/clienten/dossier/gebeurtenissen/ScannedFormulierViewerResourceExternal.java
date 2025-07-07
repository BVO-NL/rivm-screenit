package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

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
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;

@Slf4j
public class ScannedFormulierViewerResourceExternal extends AbstractResource
{
	private final boolean alsAttachement;

	private final Duration cacheDuration;

	private final String bestandReferentie;

	public ScannedFormulierViewerResourceExternal(String bestandReferentie, boolean alsAttachement)
	{
		this(bestandReferentie, alsAttachement, null);
	}

	public ScannedFormulierViewerResourceExternal(String bestandReferentie, boolean alsAttachement, Duration cacheDuration)
	{
		this.bestandReferentie = bestandReferentie;
		this.alsAttachement = alsAttachement;
		this.cacheDuration = cacheDuration;
	}

	@Override
	protected ResourceResponse newResourceResponse(Attributes attributes)
	{
		var response = new ResourceResponse();

		if (alsAttachement)
		{
			response.setContentDisposition(ContentDisposition.ATTACHMENT);
		}

		response.setFileName("image.pdf");

		if (cacheDuration != null)
		{
			response.setCacheDuration(cacheDuration);
		}

		response.setWriteCallback(new WriteCallback()
		{

			@Override
			public void writeData(Attributes attributes)
			{
				try
				{
					try (var inputStream = getInputStream())
					{
						writeStream(attributes, inputStream);
					}
				}
				catch (IOException e)
				{
					LOG.error("Fout bij laden gescande formulier: " + e.getMessage(), e);
				}
			}
		});

		return response;
	}

	protected URLConnection getUrlConnection() throws IOException
	{
		var url = new URL(bestandReferentie);
		return url.openConnection();
	}

	protected InputStream getInputStream() throws IOException
	{
		return getUrlConnection().getInputStream();
	}

	protected String getBestandReferentie()
	{
		return bestandReferentie;
	}
}
