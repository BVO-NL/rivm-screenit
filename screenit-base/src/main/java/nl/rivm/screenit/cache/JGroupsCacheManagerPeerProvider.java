package nl.rivm.screenit.cache;

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

import java.net.URL;

import javax.cache.CacheManager;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.jgroups.Global;
import org.jgroups.JChannel;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;

@Slf4j
public class JGroupsCacheManagerPeerProvider implements InitializingBean, DisposableBean
{
	private static final String CLUSTER_NAME = "EH_CACHE";

	private final CacheManager cacheManager;

	private final URL groupUrl = getClass().getResource("/jg-sit-tcp.xml");

	private final String channelName;

	private JChannel channel;

	@Getter
	private JGroupsCachePeer cachePeer;

	private JGroupsCacheEventReceiver cacheReceiver;

	public JGroupsCacheManagerPeerProvider(CacheManager cacheManager, String applicationInstance, String bindIp, Integer bindPort, String initialHosts)
	{
		this.cacheManager = cacheManager;
		try
		{
			var context = new InitialContext();
			initialHosts = (String) context.lookup("jgroupsEhcacheIPPorts");
			bindIp = (String) context.lookup("jgroupsBindIP");
			bindPort = Integer.valueOf((String) context.lookup("jgroupsEhcacheBindPort"));
			applicationInstance = (String) context.lookup("applicationInstance");
		}
		catch (NamingException e)
		{
			LOG.warn(e.getMessage());
		}

		if (StringUtils.isBlank(initialHosts))
		{
			initialHosts = "127.0.0.1[12001]";
		}
		if (StringUtils.isBlank(bindIp))
		{
			bindIp = "127.0.0.1"; 
		}
		if (bindPort == null)
		{
			bindPort = 7800;
		}
		LOG.info("External bind IP: {}; bind port: {}; initialHosts: {}", bindIp, bindPort, initialHosts);

		if (System.getProperty(Global.BIND_PORT) == null)
		{
			System.getProperties().put(Global.BIND_PORT, bindPort.toString());
		}
		if (System.getProperty(Global.EXTERNAL_ADDR) == null && !bindIp.equals("127.0.0.1") && !bindIp.equals("localhost"))
		{
			System.getProperties().put(Global.EXTERNAL_ADDR, bindIp);
		}
		if (System.getProperty(Global.GOSSIP_ROUTER) == null)
		{
			System.getProperties().put(Global.GOSSIP_ROUTER, initialHosts);
		}
		channelName = applicationInstance.toLowerCase();
	}

	@Override
	public void afterPropertiesSet()
	{
		try
		{
			channel = new JChannel(groupUrl.openStream());
		}
		catch (Exception e)
		{
			LOG.error("Failed to create JGroups Channel, replication will not function. JGroups config file: {}", groupUrl, e);
			destroy();
			return;
		}

		cachePeer = new JGroupsCachePeer(channel, CLUSTER_NAME);
		cacheReceiver = new JGroupsCacheEventReceiver(cacheManager);
		channel.setReceiver(cacheReceiver);
		channel.setDiscardOwnMessages(true);
		channel.setName(CLUSTER_NAME + "-" + channelName);

		try
		{
			channel.connect(CLUSTER_NAME);
		}
		catch (Exception e)
		{
			LOG.error("Failed to connect to JGroups cluster '{}', replication will not function. JGroups config file: {}", CLUSTER_NAME, groupUrl, e);
			destroy();
			return;
		}

		LOG.info("JGroups Replication started for '{}'. JChannel: {}", CLUSTER_NAME, channel.toString(true));
	}

	@Override
	public void destroy()
	{
		shutdownCachePeer();
		shutdownChannel();
	}

	private void shutdownCachePeer()
	{
		if (cachePeer != null)
		{
			cacheReceiver = null;
			cachePeer.dispose();
			cachePeer = null;
		}
	}

	private void shutdownChannel()
	{
		if (channel != null)
		{
			if (channel.isConnected())
			{
				try
				{
					channel.close();
					LOG.debug("Closing JChannel for cluster {}", CLUSTER_NAME);
				}
				catch (Exception e)
				{
					LOG.error("Error closing JChannel for cluster {}", CLUSTER_NAME, e);
				}
			}

			channel = null;
		}
	}
}
