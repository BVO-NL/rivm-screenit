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

import java.time.Duration;
import java.time.LocalDateTime;

import jakarta.jms.Destination;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.service.CurrentDateDispatcher;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.JmsException;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

@Service(value = "currentDateDispatcher")
@Profile("!test")
@Slf4j
public class CurrentDateDispatcherImpl implements CurrentDateDispatcher
{
	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	@Qualifier("changeDateTimeOffsetDestination")
	private Destination changeDateTimeOffsetDestination;

	@Override
	public void setOffset(Duration offset)
	{
		try
		{
			jmsTemplate.send(changeDateTimeOffsetDestination, session -> ActiveMQHelper.getActiveMqObjectMessage(offset.toString()));
		}
		catch (JmsException e)
		{
			LOG.error("Error tijdens setten van offset via activeMQ", e);
		}
	}

	@Override
	public void setNow(LocalDateTime now)
	{
		setOffset(Duration.between(LocalDateTime.now(), now));
	}
}
