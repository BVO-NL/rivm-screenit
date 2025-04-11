package nl.rivm.screenit.config;

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
import java.time.Duration;
import java.util.List;
import java.util.Set;

import javax.cache.CacheManager;
import javax.cache.Caching;

import nl.rivm.screenit.cache.JGroupsCacheEventListener;
import nl.rivm.screenit.cache.JGroupsCacheManagerPeerProvider;

import org.ehcache.config.builders.CacheConfigurationBuilder;
import org.ehcache.config.builders.ExpiryPolicyBuilder;
import org.ehcache.config.builders.ResourcePoolsBuilder;
import org.ehcache.config.units.MemoryUnit;
import org.ehcache.core.Ehcache;
import org.ehcache.core.config.DefaultConfiguration;
import org.ehcache.event.EventFiring;
import org.ehcache.event.EventOrdering;
import org.ehcache.event.EventType;
import org.ehcache.impl.config.persistence.DefaultPersistenceConfiguration;
import org.ehcache.jsr107.Eh107Configuration;
import org.ehcache.jsr107.EhcacheCachingProvider;
import org.ehcache.jsr107.config.ConfigurationElementState;
import org.ehcache.jsr107.config.Jsr107CacheConfiguration;
import org.jetbrains.annotations.NotNull;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("!test")
public class BaseCacheConfig
{

	@Bean
	public JGroupsCacheManagerPeerProvider createCachePeerProvider(ApplicationConfig applicationConfig,
		JGroupsConfig jGroupsConfig, CacheManager cacheManager)
	{
		var cacheManagerPeerProvider = new JGroupsCacheManagerPeerProvider(cacheManager, applicationConfig.applicationInstance(), jGroupsConfig.jgroupsBindIP(),
			jGroupsConfig.jgroupsEhcacheBindPort(), jGroupsConfig.jgroupsEhcacheIPPorts());

		addJGroupsEventListener(cacheManager, cacheManagerPeerProvider, "default-update-timestamps-region",

			EventType.UPDATED, EventType.REMOVED);

		var cacheNames = List.of("organisatie.cache", "verslag.cache", "screenit.cache", "colon.cache", "patient.registratie.cache", "pass.cache",
			"prefenceitem.cache", "cervix.cache", "mamma.cache");
		cacheNames.forEach(cacheName -> addJGroupsEventListener(cacheManager, cacheManagerPeerProvider, cacheName,
			EventType.UPDATED, EventType.REMOVED));

		return cacheManagerPeerProvider;
	}

	private static void addJGroupsEventListener(CacheManager cacheManager, JGroupsCacheManagerPeerProvider cacheManagerPeerProvider, String cacheName, EventType... eventTypes)
	{
		var cache = cacheManager.getCache(cacheName).unwrap(Ehcache.class);
		var listener = new JGroupsCacheEventListener(cacheName, cacheManagerPeerProvider);
		cache.getRuntimeConfiguration().registerCacheEventListener(listener, EventOrdering.ORDERED, EventFiring.SYNCHRONOUS, Set.of(eventTypes));
	}

	@Bean
	public @NotNull CacheManager getCacheManager(ApplicationConfig applicationConfig)
	{
		var cachingProvider = (EhcacheCachingProvider) Caching.getCachingProvider();
		var cacheManager = cachingProvider.getCacheManager(cachingProvider.getDefaultURI(),
			new DefaultConfiguration(cachingProvider.getDefaultClassLoader(),
				new DefaultPersistenceConfiguration(new File("ehcache" + File.separator + applicationConfig.applicationInstance().replace(" \\s", "").toLowerCase()))));
		cacheManager.createCache("nl.rivm.screenit.security.ScreenitRealm.authorizationCache", jcacheConfiguration(10000, 1800, false));
		cacheManager.createCache("nl.rivm.screenit.mamma.se.security.SERealm.authorizationCache", jcacheConfiguration(10000, 0, false));
		cacheManager.createCache("default-query-results-region", jcacheConfiguration(10000, Long.MAX_VALUE, true));
		cacheManager.createCache("default-update-timestamps-region", jcacheConfiguration(1000, Long.MAX_VALUE, true));
		cacheManager.createCache("organisatie.cache", jcacheConfiguration(10000, 900, true));
		cacheManager.createCache("verslag.cache", jcacheConfiguration(10000, 900, true));
		cacheManager.createCache("screenit.cache", jcacheConfiguration(10000, 900, true));
		cacheManager.createCache("colon.cache", jcacheConfiguration(10000, 900, true));
		cacheManager.createCache("patient.registratie.cache", jcacheConfiguration(10000, 900, true));
		cacheManager.createCache("pass.cache", jcacheConfiguration(1000, 900, true));
		cacheManager.createCache("prefenceitem.cache", jcacheConfiguration(100, 900, true));
		cacheManager.createCache("cervix.cache", jcacheConfiguration(100, 900, true));
		cacheManager.createCache("mamma.cache", jcacheConfiguration(100, 900, true));
		return cacheManager;
	}

	private javax.cache.configuration.Configuration<Object, Object> jcacheConfiguration(long entries, long ttl, boolean toDisk)
	{
		var resourcePoolsBuilder = ResourcePoolsBuilder.heap(entries);
		if (toDisk)
		{
			resourcePoolsBuilder = resourcePoolsBuilder.disk(20, MemoryUnit.MB, true).offheap(10, MemoryUnit.MB);
		}
		var cacheConfiguration = CacheConfigurationBuilder
			.newCacheConfigurationBuilder(Object.class, Object.class, resourcePoolsBuilder)
			.withExpiry(ExpiryPolicyBuilder.timeToLiveExpiration(Duration.ofSeconds(ttl)))
			.add(new Jsr107CacheConfiguration(ConfigurationElementState.ENABLED, ConfigurationElementState.DISABLED))
			.build();
		return Eh107Configuration.fromEhcacheCacheConfiguration(cacheConfiguration);
	}
}
