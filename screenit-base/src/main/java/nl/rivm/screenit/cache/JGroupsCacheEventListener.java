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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.ehcache.event.CacheEvent;
import org.ehcache.event.CacheEventListener;

@Slf4j
@AllArgsConstructor
public class JGroupsCacheEventListener implements CacheEventListener<Object, Object>
{

	private final String cacheName;

	private final JGroupsCacheManagerPeerProvider peerProvider;

	@Override
	public void onEvent(CacheEvent<?, ?> event)
	{
		LOG.trace("Getting event from {}", cacheName);

		var createPutMessage = false; 
		var message = new JGroupsCacheEventMessage<Object, Object>(
			createPutMessage ? JGroupsCacheEventMessageType.PUT : JGroupsCacheEventMessageType.REMOVE,
			event.getKey(),
			createPutMessage ? event.getNewValue() : null,
			cacheName);
		peerProvider.getCachePeer().send(message);
	}
}
