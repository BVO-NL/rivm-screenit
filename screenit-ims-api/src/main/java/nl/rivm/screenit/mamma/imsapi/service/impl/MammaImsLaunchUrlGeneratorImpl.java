package nl.rivm.screenit.mamma.imsapi.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-ims-api
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

import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.imsapi.service.MammaImsLaunchUrlGenerator;

import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;

import com.sectra.sdk.SectraUrlLaunchSharedSecret;

@Slf4j
@Service
public class MammaImsLaunchUrlGeneratorImpl implements MammaImsLaunchUrlGenerator
{
	private static final Charset CHARSET = StandardCharsets.UTF_8;

	@Override
	public String generateLoginLaunchUrl(String encryptionKey, String username, String role, boolean launchUrlSha1Mode)
	{
		return launchUrlSha1Mode ?
			generateLoginLaunchUrlSha1(encryptionKey, username, role) :
			generateLoginLaunchUrlSharedSecret(encryptionKey, username, role);
	}

	private String generateLoginLaunchUrlSharedSecret(String encryptionKey, String username, String role)
	{
		return generateLaunchUrl(encryptionKey, loginParameters(username, role));
	}

	@Override
	public String generateDesktopSyncLaunchUrl(String encryptionKey, String username, String role, String bsn, String accessionNumber, boolean launchUrlSha1Mode)
	{
		return launchUrlSha1Mode ?
			generateDesktopSyncLaunchUrlSha1(encryptionKey, username, role, bsn, accessionNumber) :
			generateDesktopSyncLaunchUrlSharedSecret(encryptionKey, username, role, bsn, accessionNumber);

	}

	private String generateDesktopSyncLaunchUrlSharedSecret(String encryptionKey, String username, String role, String bsn, String accessionNumber)
	{
		return generateLaunchUrl(encryptionKey, desktopSyncParameters(username, role, bsn, accessionNumber));
	}

	private String generateLaunchUrl(String encryptionKey, List<UrlParameter> parameters)
	{
		var urlQueryString = createUrlQueryString(parameters);
		var encryptedUrlQuery = SectraUrlLaunchSharedSecret.secure(urlQueryString, encryptionKey);

		var hostName = "localhost"; 
		var baseUrl = "sectra:?url=https://" + hostName + "/SectraHealthcareServer&softwareUrl=https://" + hostName + "/ids7/&productId=IDS7&urlQuery=";

		return baseUrl + URLEncoder.encode(encryptedUrlQuery, CHARSET);
	}

	private String createUrlQueryString(List<UrlParameter> parameters)
	{
		return parameters.stream().map(UrlParameter::toQueryString).collect(Collectors.joining("&"));
	}

	private List<UrlParameter> loginParameters(String username, String role)
	{
		return List.of(
			new UrlParameter("user_id", username),
			new UrlParameter("his_id", role),
			new UrlParameter("time", String.valueOf(Instant.now().getEpochSecond())),
			new UrlParameter("application_id", "BVO.ContextBridge"));
	}

	private ArrayList<UrlParameter> desktopSyncParameters(String username, String role, String bsn, String accessionNumber)
	{
		var parameters = new ArrayList<>(loginParameters(username, role));
		parameters.addAll(List.of(
			new UrlParameter("pat_id", bsn),
			new UrlParameter("acc_no", accessionNumber),
			new UrlParameter("exam_id", accessionNumber),
			new UrlParameter("mrn_integration_id", "Standaard"),
			new UrlParameter("acc_no_integration_id", "PACS")));
		return parameters;
	}

	private String generateLoginLaunchUrlSha1(String encryptionKey, String username, String role)
	{
		return generateLaunchUrlSha1(encryptionKey, loginParameters(username, role));
	}

	private String generateDesktopSyncLaunchUrlSha1(String encryptionKey, String username, String role, String bsn, String accessionNumber)
	{
		return generateLaunchUrlSha1(encryptionKey, desktopSyncParameters(username, role, bsn, accessionNumber));
	}

	private String generateLaunchUrlSha1(String encryptionKey, List<UrlParameter> parameters)
	{
		var stringToHash = parameters.stream().map(UrlParameter::encodedValue).collect(Collectors.joining());
		stringToHash += encryptionKey;

		var hexString = DigestUtils.sha1Hex(stringToHash);

		var baseUrl = "https://localhost/ids7/3pstart.aspx";

		return baseUrl + "?" + createUrlQueryString(parameters) + "&key=" + hexString;
	}

	private record UrlParameter(String key, String value)
	{
		String toQueryString()
		{
			return key + "=" + encodedValue();
		}

		String encodedValue()
		{
			return URLEncoder.encode(value, CHARSET);
		}
	}
}
