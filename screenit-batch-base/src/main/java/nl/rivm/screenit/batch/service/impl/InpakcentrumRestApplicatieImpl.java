package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.time.Duration;
import java.util.Base64;
import java.util.function.Consumer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.InpakcentrumRestApplicatie;
import nl.rivm.screenit.model.enums.IdpServer2ServerType;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumReadyResponseDto;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumRequestDto;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumStatusResponseDto;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumUploadRequestDto;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumUploadResponseDto;
import nl.rivm.screenit.service.IdpServer2ServerService;
import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.rivm.screenit.util.rest.RestApiFactory;

import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InpakcentrumRestApplicatieImpl implements InpakcentrumRestApplicatie
{
	@Qualifier(value = "vanScreenitNaarInpakcentrumUrl")
	private final String vanScreenitNaarInpakcentrumUrl;

	private final IdpServer2ServerService idpServer2ServerService;

	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	private <T extends InpakcentrumRequestDto, S> ResponseEntity<S> sendToInpakcentrumRestApplicatie(String context, T payload, Class<S> responseClass, HttpMethod method)
	{
		return sendToInpakcentrumRestApplicatie(context, payload, responseClass, method, Duration.ofSeconds(720));
	}

	private <T extends InpakcentrumRequestDto, S> ResponseEntity<S> sendToInpakcentrumRestApplicatie(String context, T payload, Class<S> responseClass, HttpMethod method,
		Duration readTimeout)
	{
		var restClient = RestApiFactory.createClient(readTimeout, technischeBerichtenLoggingSaverService);

		var url = context.startsWith("/") ? context : "/" + context;
		url = StringUtils.defaultIfBlank(vanScreenitNaarInpakcentrumUrl, "") + url;

		var httpHeadersConsumer = getHttpHeadersConsumer();
		return switch (method.name())
		{
			case "POST" -> restClient.post()
				.uri(url)
				.accept(MediaType.APPLICATION_JSON)
				.headers(httpHeadersConsumer)
				.body(payload)
				.retrieve()
				.toEntity(responseClass);
			case "GET" -> restClient.get()
				.uri(url)
				.accept(MediaType.APPLICATION_JSON)
				.headers(httpHeadersConsumer)
				.retrieve()
				.toEntity(responseClass);
			default -> null;
		};
	}

	private @NotNull Consumer<HttpHeaders> getHttpHeadersConsumer()
	{
		var idpAccessToken = idpServer2ServerService.getIdpAccessToken(IdpServer2ServerType.INPAKCENTRUM);
		return headers ->
		{
			if (StringUtils.isNotBlank(idpAccessToken))
			{
				headers.setBearerAuth(idpAccessToken);
			}
		};
	}

	@Override
	public InpakcentrumUploadResponseDto upload(String type, String filename, byte[] content)
	{
		var payload = new InpakcentrumUploadRequestDto();
		payload.setFilename(filename);
		payload.setDatatype(type);
		payload.setContent(content != null ? Base64.getEncoder().encodeToString(content) : null);
		return sendToInpakcentrumRestApplicatie("/upload", payload, InpakcentrumUploadResponseDto.class, HttpMethod.POST).getBody();
	}

	@Override
	public InpakcentrumReadyResponseDto ready(boolean ready)
	{
		return sendToInpakcentrumRestApplicatie("/ready?ready=" + ready, null, InpakcentrumReadyResponseDto.class, HttpMethod.GET).getBody();
	}

	@Override
	public InpakcentrumStatusResponseDto status()
	{
		return sendToInpakcentrumRestApplicatie("/status", null, InpakcentrumStatusResponseDto.class, HttpMethod.GET).getBody();
	}
}
