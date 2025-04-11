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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;

import lombok.extern.slf4j.Slf4j;

import org.jgroups.JChannel;
import org.jgroups.ObjectMessage;

@Slf4j
public class JGroupsCachePeer
{
	private static final int CHUNK_SIZE = 100;

	private final JChannel channel;

	private final ConcurrentMap<Long, Queue<JGroupsCacheEventMessage>> asyncReplicationQueues = new ConcurrentHashMap<Long, Queue<JGroupsCacheEventMessage>>();

	private final Timer timer;

	private volatile boolean alive;

	public JGroupsCachePeer(JChannel channel, String clusterName)
	{
		this.channel = channel;
		this.alive = true;
		this.timer = new Timer(clusterName + " Async Replication Thread", true);
	}

	public void dispose()
	{
		alive = false;

		disposeTimer();

		flushAllQueues();

		asyncReplicationQueues.clear();
	}

	private void disposeTimer()
	{

		timer.cancel();
		timer.purge();
	}

	public void send(JGroupsCacheEventMessage eventMessage)
	{
		if (!alive || eventMessage == null)
		{
			LOG.warn("Ignoring send request of message. Replicator alive = {}", alive);
			return;
		}

		if (eventMessage.isAsync())
		{
			var asyncTime = eventMessage.getAsyncTime();
			var queue = getMessageQueue(asyncTime);

			queue.offer(eventMessage);
			LOG.trace("Queued {} for asynchronous sending.", eventMessage);
		}
		else
		{
			LOG.trace("Sending {} synchronously.", eventMessage);
			sendData(List.of(eventMessage));
		}
	}

	private Queue<JGroupsCacheEventMessage> getMessageQueue(long asyncTime)
	{
		var queue = asyncReplicationQueues.get(asyncTime);
		if (queue == null)
		{
			var newQueue = new ConcurrentLinkedQueue<JGroupsCacheEventMessage>();
			queue = asyncReplicationQueues.putIfAbsent(asyncTime, newQueue);
			if (queue == null)
			{
				LOG.debug("Created asynchronous message queue for {}ms period", asyncTime);

				var task = new AsyncTimerTask(newQueue);
				timer.schedule(task, asyncTime, asyncTime);

				return newQueue;
			}
		}
		return queue;
	}

	private void sendData(List<? extends Serializable> dataList)
	{
		var msg = new ObjectMessage(null, dataList);
		try
		{
			channel.send(msg);
		}
		catch (IllegalStateException e)
		{
			LOG.error("Failed to send message(s) due to the channel being disconnected or closed: {}", dataList, e);
		}
		catch (Exception e)
		{
			LOG.error("Failed to send message(s) : {}", dataList, e);
		}
	}

	private void flushAllQueues()
	{
		asyncReplicationQueues.values().forEach(q -> flushQueue(q));
	}

	private void flushQueue(Queue<JGroupsCacheEventMessage> queue)
	{
		var events = new ArrayList<JGroupsCacheEventMessage>(CHUNK_SIZE);

		while (!queue.isEmpty())
		{
			events.clear();

			while (!queue.isEmpty() && events.size() < CHUNK_SIZE)
			{
				var event = queue.poll();
				if (event == null)
				{
					break;
				}

				if (event.isValid())
				{
					events.add(event);
				}
				else
				{
					LOG.warn("Collected soft reference during asynchronous queue flush, this event will not be replicated: {}", event);
				}
			}

			LOG.debug("Sending {} JGroupEventMessages from the asynchronous queue.", events.size());
			sendData(events);
		}
	}

	private final class AsyncTimerTask extends TimerTask
	{
		private final Queue<JGroupsCacheEventMessage> queue;

		private AsyncTimerTask(Queue<JGroupsCacheEventMessage> newQueue)
		{
			queue = newQueue;
		}

		@Override
		public void run()
		{
			if (!alive)
			{
				cancel();
				return;
			}

			flushQueue(queue);

			if (!alive)
			{
				cancel();
			}
		}
	}
}
