package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.security.Key;
import java.util.HashMap;
import java.util.Map;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.CommunicationHubProperties;
import nl.rivm.screenit.model.enums.IdpServer2ServerType;
import nl.rivm.screenit.service.IdpServer2ServerService;
import nl.rivm.screenit.service.KeyStoreService;
import nl.rivm.screenit.util.EnvironmentUtil;
import nl.topicuszorg.idp.client.credentials.IdpClient;
import nl.topicuszorg.idp.client.credentials.IdpClientCredentialsService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class IdpServer2ServerServiceImpl implements IdpServer2ServerService  
{

	private static final String GEEN_IDP = "geenIdp";

	private static final String AUDIENCE_ENV = "IDP_S2S_CLIENT_AUDIENCE";

	private static final String CLIENT_ID_ENV = "IDP_S2S_CLIENTID";

	private static final String KEYSTORE_LOCATION_ENV = "IDP_S2S_KEYSTORE_LOCATION";

	private static final String KEYSTORE_PASSWORD_ENV = "IDP_S2S_KEYSTORE_PASSWORD";

	private static final String DEFAULT_AUDIENCE = "https://test.login.topicuszorg.nl/auth/realms/Apps";

	private static final String DEFAULT_CLIENT_ID = "screenit-client-credentials";

	private static final String DEFAULT_KEYSTORE_LOCATION = "keystore/screenit-client-credentials-test.jks";

	private static final String DEFAULT_KEYSTORE_PASSWORD = "Dc7GsIHO7Ix7Q0tx2AAFjBejOl";

	private final KeyStoreService keyStoreService;

	private final CommunicationHubProperties communicationHubProperties;

	private final Map<IdpServer2ServerType, IdpClient> idpClients = new HashMap<>();

	private IdpClientCredentialsService idpClientCredentialsService;

	@Override
	public synchronized String getIdpAccessToken(IdpServer2ServerType type)
	{
		try
		{
			return getPrivateKeyJwtAccessToken(type);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij het opvragen Idp token voor type '{}'", type, e);
			return null;
		}
	}

	private String getPrivateKeyJwtAccessToken(IdpServer2ServerType type) throws Exception
	{
		var idpClient = idpClients.get(type);
		if (idpClient == null)
		{
			idpClient = maakIdpClient(type);
			if (idpClient == null)
			{
				return null;
			}
		}
		return getIdpClientCredentialsService().getAccessToken(idpClient);
	}

	private IdpClientCredentialsService getIdpClientCredentialsService()
	{
		if (idpClientCredentialsService == null)
		{
			idpClientCredentialsService = new IdpClientCredentialsService();
		}
		return idpClientCredentialsService;
	}

	private IdpClient maakIdpClient(IdpServer2ServerType type)
	{
		var audience = getIdpAudience(type);
		var idpTokenEndpoint = audience + "/protocol/openid-connect/token";
		var scope = getScope(type);
		IdpClient idpClient = null;
		if (!GEEN_IDP.equalsIgnoreCase(scope))
		{
			idpClient = new IdpClient(audience, getIdpClientId(type), getSigningKey(type), getClientAssertionLifespan(), idpTokenEndpoint, getExpirationOffsetSeconds(),
				scope);
			LOG.info("Idp client gemaakt voor type '{}'.", type);
		}
		else if (!idpClients.containsKey(type))
		{
			LOG.warn("Geen Idp client scope gevonden voor type '{}'. Environment variabele '{}' is niet (goed) gezet.", type, type.getScope());
		}
		idpClients.put(type, idpClient);
		return idpClient;
	}

	private Key getSigningKey(IdpServer2ServerType type)
	{
		var keystoreLocation = getKeystoreLocationOnFilestore(type);
		var keystorePassword = getKeyStorePassword(type);
		return keyStoreService.getFirstKeyFromKeyStore(keystoreLocation, keystorePassword, keystorePassword);
	}

	private String getIdpAudience(IdpServer2ServerType type)
	{
		if (type == IdpServer2ServerType.COMM_HUB)
		{
			return communicationHubProperties.getIdpAudience();
		}
		return getPropertyOfDefault(AUDIENCE_ENV, DEFAULT_AUDIENCE);
	}

	private String getIdpClientId(IdpServer2ServerType type)
	{
		if (type == IdpServer2ServerType.COMM_HUB)
		{
			return communicationHubProperties.getIdpClientId();
		}
		return getPropertyOfDefault(CLIENT_ID_ENV, DEFAULT_CLIENT_ID);
	}

	private int getClientAssertionLifespan()
	{

		return EnvironmentUtil.getIntegerEnvironmentVariable("IDP_S2S_CLIENT_ASSERTION_LIFESPAN", 10);
	}

	private int getExpirationOffsetSeconds()
	{

		return EnvironmentUtil.getIntegerEnvironmentVariable("IDP_S2S_EXPIRATION_OFFSET_SECONDS", 10);
	}

	private String getScope(IdpServer2ServerType type)
	{
		if (type == IdpServer2ServerType.COMM_HUB)
		{
			return communicationHubProperties.getIdpScope();
		}
		return getPropertyOfDefault(type.getScope(), GEEN_IDP);
	}

	private String getKeystoreLocationOnFilestore(IdpServer2ServerType type)
	{
		if (type == IdpServer2ServerType.COMM_HUB)
		{
			return communicationHubProperties.getIdpKeystoreLocation();
		}
		return getPropertyOfDefault(KEYSTORE_LOCATION_ENV, DEFAULT_KEYSTORE_LOCATION);
	}

	private String getKeyStorePassword(IdpServer2ServerType type)
	{

		if (type == IdpServer2ServerType.COMM_HUB)
		{
			return communicationHubProperties.getIdpKeystorePassword();
		}
		return getPropertyOfDefault(KEYSTORE_PASSWORD_ENV, DEFAULT_KEYSTORE_PASSWORD);
	}

	private String getPropertyOfDefault(String key, String defaultValue)
	{
		if (StringUtils.isBlank(key))
		{
			return defaultValue;
		}
		return EnvironmentUtil.getStringEnvironmentVariable(key, defaultValue);
	}
}
