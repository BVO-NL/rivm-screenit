package nl.rivm.screenit.model.helper;

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

import java.io.Serializable;

import jakarta.jms.JMSException;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.util.ScreenitObjectMapper;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.apache.activemq.command.ActiveMQTextMessage;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ActiveMQHelper
{
	private static final ObjectWriter OBJECT_WRITER = new ScreenitObjectMapper().writer();

	private static final ObjectWriter TRACE_WRITER = OBJECT_WRITER.without(SerializationFeature.FAIL_ON_EMPTY_BEANS);

	public static <T extends Serializable> ActiveMQObjectMessage getActiveMqObjectMessage(T object) throws JMSException
	{
		if (LOG.isTraceEnabled())
		{
			try
			{
				LOG.trace("{}: {}", object.getClass().getSimpleName(), TRACE_WRITER.writeValueAsString(object));
			}
			catch (JsonProcessingException e)
			{
				LOG.error("Fout bij maken JSON voor ActiveMQObjectMessage bericht", e);
			}
		}
		var objectMessage = new ActiveMQObjectMessage();
		objectMessage.setObject(object);
		return objectMessage;
	}

	public static <T extends Serializable> ActiveMQTextMessage getActiveMqTextMessage(T object) throws JMSException
	{
		try
		{
			var textMessage = new ActiveMQTextMessage();
			textMessage.setText(OBJECT_WRITER.writeValueAsString(object));
			return textMessage;
		}
		catch (JsonProcessingException e)
		{
			var jmsException = new JMSException("Fout bij maken JSON voor activemq text bericht van " + object.getClass().getSimpleName());
			jmsException.setLinkedException(e);
			jmsException.initCause(e);
			throw jmsException;
		}
	}
}
