package nl.rivm.screenit.mamma.imsapi.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-ims-api
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

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;

import nl.rivm.screenit.mamma.imsapi.service.MammaImsLaunchUrlGenerator;

import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.util.UriUtils;

import com.sectra.sdk.SectraUrlLaunchSharedSecret;

@Service
public class MammaImsLaunchUrlGeneratorImpl implements MammaImsLaunchUrlGenerator
{
	private static final String HOST_NAME = "localhost"; 

	@Override
	public String generateLoginLaunchUrl(String encryptionKey, String username, String role)
	{
		return generateLaunchUrl(encryptionKey, loginParameters(username, role));
	}

	@Override
	public String generateDesktopSyncLaunchUrl(String encryptionKey, String username, String role, String bsn, String accessionNumber)
	{
		return generateLaunchUrl(encryptionKey, desktopSyncParameters(username, role, bsn, accessionNumber));
	}

	private String generateLaunchUrl(String encryptionKey, Map<String, String> parameters)
	{
		var encryptedUrlQuery = SectraUrlLaunchSharedSecret.secure(maakUrlQuery(parameters), encryptionKey);

		return UriComponentsBuilder.newInstance()
			.scheme("sectra")
			.queryParam("url", maakServerUrl("/SectraHealthcareServer"))
			.queryParam("softwareUrl", maakServerUrl("/ids7/"))
			.queryParam("productId", "IDS7")
			.queryParam("urlQuery", encryptedUrlQuery)
			.build()
			.encode(StandardCharsets.UTF_8)
			.toUriString();
	}

	private String maakUrlQuery(Map<String, String> parameters)
	{
		var builder = UriComponentsBuilder.newInstance();
		parameters.forEach((key, value) -> builder.queryParam(key, URLEncoder.encode(value, StandardCharsets.UTF_8)));
		return builder.build(true).getQuery();
	}

	private String maakServerUrl(String path)
	{
		return UriComponentsBuilder.newInstance()
			.scheme("https")
			.host(HOST_NAME)
			.path(path)
			.toUriString();
	}

	private Map<String, String> loginParameters(String username, String role)
	{
		var parameters = new LinkedHashMap<String, String>();
		parameters.put("user_id", username);
		parameters.put("his_id", role);
		parameters.put("time", String.valueOf(Instant.now().getEpochSecond()));
		parameters.put("application_id", "BVO.ContextBridge");
		return parameters;
	}

	private Map<String, String> desktopSyncParameters(String username, String role, String bsn, String accessionNumber)
	{
		var parameters = loginParameters(username, role);
		parameters.put("pat_id", bsn);
		parameters.put("acc_no", accessionNumber);
		parameters.put("exam_id", accessionNumber);
		parameters.put("mrn_integration_id", "Standaard");
		parameters.put("acc_no_integration_id", "PACS");
		return parameters;
	}
}
