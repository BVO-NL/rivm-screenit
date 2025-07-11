package nl.rivm.screenit.main.service.impl;

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

import java.io.FileInputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import jakarta.inject.Inject;

import javax.net.ssl.SSLContext;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.IdpServer2ServerService;
import nl.rivm.screenit.main.service.ZorgIdSessieService;
import nl.rivm.screenit.main.service.impl.zorgid.ClosedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.InitializedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.OpenCancelledSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.OpenedSessieState;
import nl.rivm.screenit.main.service.impl.zorgid.SessieState;
import nl.topicuszorg.cloud.distributedsessions.RedisConfig;
import nl.topicuszorg.cloud.distributedsessions.jedis.JedisFactory;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernateSession;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.zorgid.client.ZorgidClient;
import nl.topicuszorg.zorgid.client.impl.ZorgidClientImpl;
import nl.topicuszorg.zorgid.model.ZorgidException;
import nl.topicuszorg.zorgid.model.sessie.ClosedReason;
import nl.topicuszorg.zorgid.model.sessie.OpenCancelledReason;
import nl.topicuszorg.zorgid.webservice.SessionInitializedEvent;

import org.apache.commons.lang3.StringUtils;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManagerBuilder;
import org.apache.hc.client5.http.ssl.SSLConnectionSocketFactory;
import org.apache.hc.core5.http.io.SocketConfig;
import org.apache.hc.core5.util.TimeValue;
import org.apache.hc.core5.util.Timeout;
import org.apache.http.ssl.SSLContexts;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;

@Configuration
@EnableScheduling
public class ZorgIdSessieServiceImpl implements ZorgIdSessieService, ApplicationListener<ContextRefreshedEvent>
{
	private static final Logger LOG = LoggerFactory.getLogger(ZorgIdSessieServiceImpl.class);

	private static final int MAX_CONCURRENT_CONNECTIONS = 16;

	private static final int SESSIE_TIMEOUT = 1800; 

	private static final ObjectMapper objectMapper = new ObjectMapper();

	private static final long ZORG_ID_CONNECTION_TIME = 300L;

	@Inject
	private SimplePreferenceService preferenceService;

	@Inject
	private IdpServer2ServerService idpService;

	@Inject
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	@Inject
	@Qualifier("applicationInstance")
	private String applicationInstance;

	private ZorgidClient zorgidClient;

	private final Map<UUID, SessieState> activeSessions = new ConcurrentHashMap<>();

	private boolean doInit = true;

	private RedisConfig redisConfig;

	private JedisPool jedisPool;

	private int redisErrors = 0;

	@JsonIgnoreProperties(value = { "expired" })
	private class MixInByPropName
	{
	}

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit)
		{
			objectMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
			objectMapper.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.NONE);
			objectMapper.addMixIn(Object.class, MixInByPropName.class);
			LOG.info("doInit");
			OpenHibernateSession.withCommittedTransaction().run(() ->
			{
				try
				{
					this.zorgidClient = zorgidClient(zorgidClientTemplate());
				}
				catch (Exception e)
				{
					LOG.error("Fout bij opstarten", e);
				}
				finally
				{
					doInit = false;
				}
			});

		}
	}

	@Override
	public UUID startSessie() throws ZorgidException
	{
		final UUID uuid = zorgidClient.start();
		LOG.info("Open detached: {}", uuid);
		return uuid;
	}

	@Override
	public SessieState getSessieState(UUID uuid)
	{
		JedisPool jedisPool = getJedisPool();
		if (jedisPool != null)
		{
			return getCachedObject(uuid);
		}
		return activeSessions.get(uuid);
	}

	private SessieState getCachedObject(UUID uuid)
	{
		Jedis jedis = null;
		try
		{
			String key = getKey(uuid);
			jedis = jedisPool.getResource();
			SessieState obj = null;
			String serialized = jedis.get(key);
			if (StringUtils.isNotBlank(serialized))
			{
				obj = fromString(serialized);
			}
			return obj;
		}
		catch (Exception e)
		{
			logJedisError(e);
			return null;
		}
		finally
		{
			closeJedis(jedis);
		}
	}

	private static String getKey(UUID uuid)
	{
		String key = "ZorgId-" + uuid.toString();
		if (LOG.isTraceEnabled())
		{
			LOG.trace("Key: {}", key);
		}
		return key;
	}

	private static SessieState fromString(String s) throws IOException, ClassNotFoundException
	{
		String[] splittedString = s.split("\\|");
		Object object = objectMapper.readValue(splittedString[1], Class.forName(splittedString[0]));
		LOG.trace(object.getClass().getSimpleName());
		return (SessieState) object;
	}

	private static String toString(SessieState sessieState)
	{
		String sessieStateString = null;
		try
		{
			sessieStateString = objectMapper.writeValueAsString(sessieState);
			LOG.trace("{}: {}", sessieState.getClass().getSimpleName(), sessieStateString);
		}
		catch (JsonProcessingException e)
		{
			LOG.error("Fout bij maken JSON voor ActiveMQObjectMessage bericht", e);
		}
		return sessieState.getClass().getName() + "|" + sessieStateString;
	}

	private void storeCacheObject(UUID uuid, SessieState sessieState)
	{
		Jedis jedis = null;
		try
		{
			String key = getKey(uuid);
			jedis = jedisPool.getResource();
			jedis.set(key, toString(sessieState));
			jedis.expire(key, SESSIE_TIMEOUT);
		}
		catch (Exception e)
		{
			logJedisError(e);
		}
		finally
		{
			closeJedis(jedis);
		}
	}

	@Override
	public void refreshSessie(UUID uuid)
	{
		JedisPool jedisPool = getJedisPool();
		if (jedisPool != null && uuid != null)
		{
			Jedis jedis = null;
			try
			{
				String key = getKey(uuid);
				jedis = jedisPool.getResource();
				jedis.expire(key, SESSIE_TIMEOUT);
			}
			catch (Exception e)
			{
				logJedisError(e);
			}
			finally
			{
				closeJedis(jedis);
			}
		}
	}

	@Override
	public void stopSessie(UUID uuid) throws ZorgidException
	{
		LOG.info("Close: {}", uuid);
		zorgidClient.close(uuid);
	}

	@Scheduled(cron = "0 */5 * * * ?")
	public void removeVerlopenSessies()
	{

		activeSessions.entrySet().removeIf(entry ->
		{
			final boolean remove = entry.getValue().isExpired();
			if (remove)
			{
				if (entry.getValue() instanceof InitializedSessieState)
				{
					LOG.warn("Verlopen InitializedSessieState wordt verwijderd (geen timeout van Zorg-ID?): {}", entry.getKey());
				}
				else if (entry.getValue() instanceof OpenedSessieState)
				{
					LOG.warn("Verlopen OpenedSessieState wordt verwijderd (gebruiker te lang ingelogd?): {}", entry.getKey());
				}
				else
				{
					LOG.debug("Verlopen sessie state wordt verwijderd: {}", entry.getKey());
				}
			}
			return remove;
		});
	}

	@Override
	public void onSessionInitialized(SessionInitializedEvent sessionInitializedEvent)
	{
		var uuid = sessionInitializedEvent.getUuid();
		LOG.info("Session initialized: {}", uuid);
		addSessieState(uuid, new InitializedSessieState(sessionInitializedEvent));
	}

	@Override
	public void onSessionOpened(UUID uuid, String secureElement)
	{
		LOG.info("Session open: {}", uuid);
		addSessieState(uuid, new OpenedSessieState(secureElement));
	}

	@Override
	public void onSessionOpenCancelled(UUID uuid, OpenCancelledReason reason)
	{
		LOG.info("Session open cancelled: {}, reason: {}", uuid, reason);
		addSessieState(uuid, new OpenCancelledSessieState(reason));
	}

	@Override
	public void onSessionClosed(UUID uuid, ClosedReason reason)
	{
		LOG.info("Session closed: {}, reason: {}", uuid, reason);
		addSessieState(uuid, new ClosedSessieState(reason));
	}

	private void addSessieState(UUID uuid, SessieState sessieState)
	{
		JedisPool jedisPool = getJedisPool();
		if (jedisPool != null)
		{
			storeCacheObject(uuid, sessieState);
		}
		activeSessions.put(uuid, sessieState);
	}

	private JedisPool getJedisPool()
	{
		if (jedisPool == null && redisConfig != null && redisConfig.isEnabled())
		{

			jedisPool = JedisFactory.createJedisPool(true, MAX_CONCURRENT_CONNECTIONS, redisConfig).orElseGet(null);
		}
		return jedisPool;
	}

	private String zorgidCallbackUrl()
	{
		return String.format(getStringValue(PreferenceKey.INTERNAL_ZORGID_CALLBACKURL, "http://localhost:14800/rest/zorgid"), applicationInstance.toLowerCase());
	}

	private String zorgidServerUrl()
	{
		return getStringValue(PreferenceKey.INTERNAL_ZORGID_SERVERURL, "http://localhost:8080");
	}

	private int zorgidClientReadTimeout()
	{
		return getIntegerValue(PreferenceKey.INTERNAL_ZORGID_CLIENTCONNECTTIMEOUT, 500);
	}

	private int zorgidClientConnectTimeout()
	{
		return getIntegerValue(PreferenceKey.INTERNAL_ZORGID_CLIENTCONNECTTIMEOUT, 200);
	}

	private int zorgidClientMaxConnections()
	{
		return getIntegerValue(PreferenceKey.INTERNAL_ZORGID_CLIENTMAXCONNECTIONS, 10);
	}

	private String zorgidKeyStoreLocation()
	{
		return getStringValue(PreferenceKey.INTERNAL_ZORGID_WEBSERVICEKEYSTORE, ZorgIdSessieServiceImpl.class.getResource("/zorgid-client-development.jks").getFile());
	}

	private char[] zorgidKeyStorePassword()
	{
		return getStringValue(PreferenceKey.INTERNAL_ZORGID_WEBSERVICEKEYSTOREPASSWORD, "zorgid-client").toCharArray();
	}

	private String getStringValue(PreferenceKey prefKey, String defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getString(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	private int getIntegerValue(PreferenceKey prefKey, Integer defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getInteger(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	private boolean useDbPreferences()
	{
		return !"Filler".equals(applicationEnvironment) && !"Unittest".equals(applicationEnvironment);
	}

	private SSLContext zorgidSslContext()
	{
		final String keyStoreLocation = zorgidKeyStoreLocation();
		final char[] keyStorePassword = zorgidKeyStorePassword();

		try (final FileInputStream is = new FileInputStream(keyStoreLocation))
		{
			final KeyStore keystore = KeyStore.getInstance(KeyStore.getDefaultType());
			keystore.load(is, keyStorePassword);

			return SSLContexts.custom()
				.loadKeyMaterial(keystore, keyStorePassword)
				.build();
		}
		catch (IOException | GeneralSecurityException e)
		{
			throw new IllegalStateException("Kon de Zorg-ID key store niet laden", e);
		}
	}

	private RestTemplate zorgidClientTemplate()
	{
		var maxConnPerRoute = zorgidClientMaxConnections();
		var socketConfig = SocketConfig.custom()
			.setSoTimeout(Timeout.ofMilliseconds(zorgidClientReadTimeout())).build();
		var httpClientConnectionManager = PoolingHttpClientConnectionManagerBuilder.create()
			.setMaxConnPerRoute(maxConnPerRoute)
			.setMaxConnTotal(2 * maxConnPerRoute)
			.setDefaultSocketConfig(socketConfig)
			.setConnectionTimeToLive(TimeValue.ofSeconds(ZORG_ID_CONNECTION_TIME))
			.setSSLSocketFactory(new SSLConnectionSocketFactory(zorgidSslContext()))
			.build();
		var httpClient = HttpClients.custom()
			.setConnectionManager(httpClientConnectionManager)
			.build();
		var httpRequestFactory = new HttpComponentsClientHttpRequestFactory(httpClient);
		httpRequestFactory.setConnectionRequestTimeout(zorgidClientConnectTimeout());
		httpRequestFactory.setConnectTimeout(zorgidClientConnectTimeout());
		return new RestTemplate(httpRequestFactory);
	}

	private ZorgidClient zorgidClient(RestTemplate zorgidClientTemplate)
	{
		String zorgidCallbackUrl = zorgidCallbackUrl();
		LOG.info("Opgestart met zorgidCallbackUrl: {}", zorgidCallbackUrl);
		Supplier<String> oAuthTokenSupplier = idpService::getIdpAccessTokenVoorZorgId;
		return new ZorgidClientImpl(zorgidServerUrl(), zorgidCallbackUrl, zorgidClientTemplate, oAuthTokenSupplier);
	}

	@Override
	public void setRedisConfig(RedisConfig redisConfig)
	{
		this.redisConfig = redisConfig;
	}

	private void logJedisError(Exception e)
	{

		if (redisErrors < 100 || redisErrors % 100000 == 0)
		{
			LOG.error(e.getMessage(), e);
			if (redisErrors > 100)
			{
				LOG.error("\n*****\nCheck if Jedis server is down. No cache is being used.\n*****\n");

				redisErrors = 0;
			}
		}
		redisErrors++;
	}

	private void closeJedis(Jedis jedis)
	{
		if (jedis != null)
		{
			jedis.close();
		}
	}
}
