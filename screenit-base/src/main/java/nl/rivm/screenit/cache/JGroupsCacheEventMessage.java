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

import java.io.IOException;
import java.io.Serializable;
import java.lang.ref.SoftReference;

import lombok.Getter;

public class JGroupsCacheEventMessage<K, V> implements Serializable
{
	private static final long serialVersionUID = 1L;

	private static final long DEFAULT_ASYNC_INTERVAL = 1000;

	@Getter
	private final String cacheName;

	@Getter
	private final long asyncTime;

	@Getter
	private final K key;

	@Getter
	private final JGroupsCacheEventMessageType event;

	private transient SoftReference valueSoftReference;

	private final boolean wasValueNotNull;

	public JGroupsCacheEventMessage(JGroupsCacheEventMessageType event, K key, V value, String cacheName, long asyncTime)
	{
		this.cacheName = cacheName;
		this.asyncTime = asyncTime;
		this.key = key;
		this.event = event;
		wasValueNotNull = value != null;
		valueSoftReference = new SoftReference(value);
	}

	public JGroupsCacheEventMessage(JGroupsCacheEventMessageType event, K key, V value, String cacheName)
	{
		this(event, key, value, cacheName, DEFAULT_ASYNC_INTERVAL);
	}

	public boolean isAsync()
	{
		return this.asyncTime >= 0;
	}

	public final V getValue()
	{
		return (V) valueSoftReference.get();
	}

	public boolean isValid()
	{
		if (!wasValueNotNull)
		{
			return true;
		}
		else
		{
			return getValue() != null;
		}
	}

	private void writeObject(java.io.ObjectOutputStream out) throws IOException
	{
		out.defaultWriteObject();
		V value = getValue();
		out.writeObject(value);
	}

	private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException
	{
		in.defaultReadObject();
		V value = (V) in.readObject();
		valueSoftReference = new SoftReference(value);
	}

	@Override
	public String toString()
	{
		return "JGroupsCacheEventMessage [event=" + event +
			", cacheName=" + cacheName +
			", serializableKey=" + key +
			", value=" + getValue() + "]";
	}

}
