package nl.rivm.screenit.util.rest;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.util.StreamUtils;

final class BufferingClientHttpResponseWrapper implements ClientHttpResponse
{

	private final ClientHttpResponse response;

	private byte[] body;

	BufferingClientHttpResponseWrapper(ClientHttpResponse response)
	{
		this.response = response;
	}

	@Override
	public @NotNull HttpStatusCode getStatusCode() throws IOException
	{
		return response.getStatusCode();
	}

	@Override
	public @NotNull String getStatusText() throws IOException
	{
		return response.getStatusText();
	}

	@Override
	public @NotNull HttpHeaders getHeaders()
	{
		return response.getHeaders();
	}

	@Override
	public @NotNull InputStream getBody() throws IOException
	{
		if (body == null)
		{
			body = StreamUtils.copyToByteArray(response.getBody());
		}
		return new ByteArrayInputStream(body);
	}

	@Override
	public void close()
	{
		response.close();
	}

}
