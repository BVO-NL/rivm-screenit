package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import jakarta.jms.JMSException;
import jakarta.jms.Session;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.LeegDossierService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@Setter
public class JMSDossierLegenListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	private LeegDossierService dossierService;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session)
	{
		try
		{
			var clientId = (Long) message.getObject();
			dossierService.maakDossierLeeg(clientId);
		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}
}
