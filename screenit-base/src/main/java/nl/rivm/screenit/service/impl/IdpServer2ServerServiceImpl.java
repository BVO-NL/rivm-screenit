package nl.rivm.screenit.service.impl;

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

import java.io.File;
import java.security.Key;
import java.util.HashMap;
import java.util.Map;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.enums.IdpServer2ServerType;
import nl.rivm.screenit.service.IdpServer2ServerService;
import nl.rivm.screenit.service.KeyStoreService;
import nl.rivm.screenit.util.EnvironmentUtil;
import nl.topicuszorg.idp.client.credentials.IdpClient;
import nl.topicuszorg.idp.client.credentials.IdpClientCredentialsService;

import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class IdpServer2ServerServiceImpl implements IdpServer2ServerService
{

	private static final String GEEN_IDP = "geenIdp";

	private final KeyStoreService keyStoreService;

	private IdpClientCredentialsService idpClientCredentialsService;

	private final Map<IdpServer2ServerType, IdpClient> idpClients = new HashMap<>();

	@Override
	public synchronized String getIdpAccessToken(IdpServer2ServerType type)
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
		try
		{
			return getIdpClientCredentialsService().getAccessToken(idpClient);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij het opvragen Idp token voor type '{}'", type, e);
			return null;
		}
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
		var audience = getIdpAudience();
		var idpTokenEndpoint = audience + "/protocol/openid-connect/token";
		var scope = getScope(type);
		IdpClient idpClient = null;
		if (!GEEN_IDP.equalsIgnoreCase(scope))
		{
			idpClient = new IdpClient(audience, getIdpClientId(), getSigningKey(), getClientAssertionLifespan(), idpTokenEndpoint, getExpirationOffsetSeconds(), scope);
			LOG.info("Idp client gemaakt voor type '{}'.", type);
		}
		else if (!idpClients.containsKey(type))
		{
			LOG.warn("Geen Idp client scope gevonden voor type '{}'. Environment variabele '{}' is niet (goed) gezet.", type, type.getScope());
		}
		idpClients.put(type, idpClient);
		return idpClient;
	}

	private Key getSigningKey()
	{
		return keyStoreService.getFirstKeyFromKeyStore(getKeystoreLocationOnFilestore(), getKeyStorePassword(), getKeyStorePassword());
	}

	private String getIdpAudience()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_CLIENT_AUDIENCE", "https://test.login.topicuszorg.nl/auth/realms/Apps");
	}

	private String getIdpClientId()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_CLIENTID", "screenit-client-credentials"); 
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
		return EnvironmentUtil.getStringEnvironmentVariable(type.getScope(), GEEN_IDP); 
	}

	private String getKeystoreLocationOnFilestore()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_KEYSTORE_LOCATION", "keystore" + File.separator + "screenit-client-credentials-test.jks");
	}

	private String getKeyStorePassword()
	{

		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_KEYSTORE_PASSWORD", "Dc7GsIHO7Ix7Q0tx2AAFjBejOl");
	}
}
