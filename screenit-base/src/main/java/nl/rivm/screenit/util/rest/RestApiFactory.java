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

import java.net.http.HttpClient;
import java.time.Duration;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;

import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.apache.hc.core5.http.io.SocketConfig;
import org.apache.hc.core5.util.Timeout;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.client.JdkClientHttpRequestFactory;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestTemplate;

public final class RestApiFactory
{
	private RestApiFactory()
	{

	}

	public static RestTemplate create()
	{
		return create(null);
	}

	public static RestTemplate create(Duration readTimeout)
	{
		var connectionManager = new PoolingHttpClientConnectionManager();

		if (readTimeout == null)
		{
			readTimeout = Duration.ofMinutes(150); 
		}
		connectionManager.setDefaultSocketConfig(SocketConfig.custom()
			.setSoTimeout(Timeout.of(readTimeout))
			.build());

		var httpClient = HttpClientBuilder.create()
			.setConnectionManager(connectionManager)
			.build();

		return new RestTemplateBuilder()
			.requestFactory(() -> new HttpComponentsClientHttpRequestFactory(httpClient))
			.errorHandler(new ScreenitRestErrorHandler())
			.additionalInterceptors(new LoggingRequestInterceptor())
			.build();
	}

	public static RestClient createClient(Duration readTimeout)
	{
		return createClient(readTimeout, null);
	}

	public static RestClient createClient(Duration readTimeout, TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		if (readTimeout == null)
		{
			readTimeout = Duration.ofMinutes(150); 
		}

		var httpClient = HttpClient.newBuilder()
			.connectTimeout(readTimeout)
			.build();

		var builder = RestClient.builder()
			.requestFactory(new JdkClientHttpRequestFactory(httpClient));
		if (technischeBerichtenLoggingSaverService != null)
		{
			builder.requestInterceptor(new TechnischeLoggingRequestOutInterceptor(technischeBerichtenLoggingSaverService));
		}
		return builder.build();
	}
}
