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
import java.nio.charset.StandardCharsets;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;

import org.apache.commons.io.IOUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;

@Slf4j
public class TechnischeLoggingRequestOutInterceptor implements ClientHttpRequestInterceptor
{
	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	public TechnischeLoggingRequestOutInterceptor(TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		this.technischeBerichtenLoggingSaverService = technischeBerichtenLoggingSaverService;
	}

	@Override
	public @NotNull ClientHttpResponse intercept(@NotNull HttpRequest request, byte @NotNull [] body, @NotNull ClientHttpRequestExecution execution) throws IOException
	{
		ClientHttpResponse responseCopy;
		long exchangeId = 0L;
		try
		{
			exchangeId = technischeBerichtenLoggingSaverService.logRequest("REST_REQ_OUT",
				"Method:" + request.getMethod() + "|URI:" + request.getURI(),
				new String(body, StandardCharsets.UTF_8));
			var response = execution.execute(request, body);
			responseCopy = new BufferingClientHttpResponseWrapper(response);
			var responseBody = "";
			var statusCode = responseCopy.getStatusCode();
			if (statusCode == HttpStatus.OK || statusCode == HttpStatus.INTERNAL_SERVER_ERROR)
			{
				responseBody = IOUtils.toString(responseCopy.getBody(), Charset.defaultCharset());
			}
			technischeBerichtenLoggingSaverService.logResponse("REST_RESP_IN", exchangeId, responseBody + "|Status:" + statusCode);
		}
		catch (Exception e)
		{
			technischeBerichtenLoggingSaverService.logResponse("REST_FAULT_IN", exchangeId, e.getMessage());
			LOG.error("Error in request: ", e);
			throw e;
		}
		return responseCopy;
	}
}
