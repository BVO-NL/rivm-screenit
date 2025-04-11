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

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import javax.cache.CacheManager;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.jgroups.Message;
import org.jgroups.Receiver;
import org.jgroups.View;

@Slf4j
@AllArgsConstructor
public class JGroupsCacheEventReceiver implements Receiver
{

	private final CacheManager cacheManager;

	@Override
	public void receive(Message msg)
	{
		if (msg == null || msg.getLength() == 0)
		{
			LOG.warn("Received an empty or null Message");
			return;
		}

		var object = msg.getObject();
		if (object == null)
		{
			LOG.warn("Received a Message with a null object");
			return;
		}

		if (object instanceof JGroupsCacheEventMessage jGroupsCacheEventMessage)
		{
			safeHandleJGroupNotification(jGroupsCacheEventMessage);
		}
		else if (object instanceof List<?> messages)
		{
			LOG.trace("Received List of {} JGroupEventMessages", messages.size());

			for (var message : messages)
			{
				if (message == null)
				{
					continue;
				}

				if (message instanceof JGroupsCacheEventMessage jGroupsCacheEventMessage)
				{
					safeHandleJGroupNotification(jGroupsCacheEventMessage);
				}
				else
				{
					LOG.warn("Received message of type {} but member was of type '{}' and not {}. Member ignored: {}", List.class, message.getClass(),
						JGroupsCacheEventMessage.class, message);
				}
			}
		}
		else
		{
			LOG.warn("Received message with payload of type {} and not {} or List<{}>.", object.getClass(), JGroupsCacheEventMessage.class,
				JGroupsCacheEventMessage.class.getSimpleName());
		}
	}

	private void safeHandleJGroupNotification(JGroupsCacheEventMessage message)
	{
		try
		{
			var cacheName = message.getCacheName();
			if (cacheName == null)
			{
				LOG.warn("Received message with null cacheName, ignoring message: {}", message);
				return;
			}
			handleEhcacheNotification(message, cacheName);
		}
		catch (Exception e)
		{
			LOG.error("Failed to handle message {}", message, e);
		}
	}

	private void handleEhcacheNotification(JGroupsCacheEventMessage message, final String cacheName)
	{
		var cache = cacheManager.getCache(cacheName);
		if (cache == null)
		{
			LOG.warn("Received message for cache that does not exist: {}", cacheName);
			return;
		}

		switch (message.getEvent())
		{
		case REMOVE:
		{
			var key = message.getKey();
			if (cache.get(key) != null)
			{
				LOG.debug("received remove:  cache={}, key={}", cacheName, key);
				cache.remove(key);
			}
			else if (LOG.isTraceEnabled())
			{
				LOG.trace("received remove:  cache={}, key={} - Ignoring, key is not in the local cache.",
					cacheName, key);
			}
			break;
		}
		case PUT:
		{
			var key = message.getKey();
			LOG.debug("received put:     cache={}, key={}", cacheName, key);
			cache.put(key, message.getValue());
			break;
		}
		default:
		{
			LOG.warn("Unknown JGroupsCacheEventMessage type received, ignoring message: {}", message);
			break;
		}
		}
	}

	@Override
	public void getState(OutputStream output)
	{

	}

	@Override
	public void setState(InputStream input)
	{

	}

	@Override
	public void viewAccepted(View newView)
	{

	}
}
