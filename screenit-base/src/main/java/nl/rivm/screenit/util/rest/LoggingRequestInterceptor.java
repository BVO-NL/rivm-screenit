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

import java.io.IOException;
import java.nio.charset.Charset;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.io.IOUtils;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;

@Slf4j
public class LoggingRequestInterceptor implements ClientHttpRequestInterceptor
{

	@Override
	public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution) throws IOException
	{
		ClientHttpResponse response = execution.execute(request, body);
		ClientHttpResponse responseCopy = response;
		if (LOG.isTraceEnabled())
		{
			responseCopy = new BufferingClientHttpResponseWrapper(response);
			String responseBody = "";
			if (responseCopy.getStatusCode() == HttpStatus.OK || responseCopy.getStatusCode() == HttpStatus.INTERNAL_SERVER_ERROR)
			{
				responseBody = IOUtils.toString(responseCopy.getBody(), Charset.defaultCharset());
			}

			LOG.trace("Method: {} | URI: {} | Request: {} | Response: {}", request.getMethod(), request.getURI(), new String(body), responseBody);
		}
		return responseCopy;
	}
}
