package nl.rivm.screenit.main.jms.listener;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

import jakarta.jms.JMSException;
import jakarta.jms.Session;

@Slf4j
@Component
public class HuisartsportaalListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{

	@Autowired
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@Override
	public void onMessage(ActiveMQObjectMessage objectMessage, Session session)
	{
		try
		{
			String loginfo = "Synchronisatie bericht " + objectMessage.getJMSMessageID();
			Object object = objectMessage.getObject();

			if (object instanceof AanvraagDto aanvraagDto)
			{
				LOG.info("{} voor type aanvraag(ha_id: {}, s_id: {})", loginfo, aanvraagDto.getHuisartsportaalId(), aanvraagDto.getScreenitId());
				cervixHuisartsSyncService.setLabformulierAanvraag(aanvraagDto);
			}
			else if (object instanceof LocatieDto locatieDto)
			{
				LOG.info("{} voor type locatie(ha_id: {}, s_id: {})", loginfo, locatieDto.getHuisartsportaalId(), locatieDto.getScreenitId());
				cervixHuisartsSyncService.updateLocatie(locatieDto);
			}
			else if (object instanceof HuisartsDto huisartsDto)
			{
				LOG.info("{} voor type huisarts(ha_id: {}, s_id: {})", loginfo, huisartsDto.getHuisartsportaalId(), huisartsDto.getScreenitId());
				cervixHuisartsSyncService.updateHuisarts(huisartsDto);
			}
			LOG.info("Bericht succesvol verwerkt: {}", objectMessage.getJMSMessageID());
		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}
}
