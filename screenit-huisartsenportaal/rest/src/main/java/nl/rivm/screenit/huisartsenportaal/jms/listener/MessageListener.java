package nl.rivm.screenit.huisartsenportaal.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.OvereenkomstDto;
import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingDto;
import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.LabformulierService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.OvereenkomstService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;
import nl.rivm.screenit.huisartsenportaal.service.WoonplaatsService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;

import jakarta.jms.JMSException;
import jakarta.jms.Message;

@Component
public class MessageListener
{

	private static final Logger LOG = LoggerFactory.getLogger(MessageListener.class);

	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private OvereenkomstService overeenkomstService;

	@Autowired
	private LabformulierService labformulierService;

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private VerrichtingenService verrichtingenService;

	@Autowired
	private AuthenticatieService authenticatieService;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@JmsListener(containerFactory = "jmsListenerContainerFactory", destination = "nl.rivm.screenit.huisartsportaal.${app.environment}")
	private void newMessage(Message message)
	{
		try
		{
			String loginfo = "Synchronisatie bericht " + message.getJMSMessageID();
			if (message instanceof ActiveMQObjectMessage objectMessage)
			{
				Object object = objectMessage.getObject();

				if (object instanceof HuisartsDto dto)
				{
					LOG.info("{} voor type huisarts(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());
					huisartsService.updateAndGetHuisarts(dto);
				}

				else if (object instanceof LocatieDto dto)
				{
					Huisarts huisarts = huisartsService.getHuisartsWith(dto.getHuisartsId());
					LOG.info("{} voor type locatie(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());

					locatieService.updateAndGetLocatie(huisarts, dto);
					locatieService.nietVerstuurdeLabformulierenVerwijderen(dto);
				}
				else if (object instanceof OvereenkomstDto dto)
				{
					LOG.info("{} voor type overeenkomst(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());
					overeenkomstService.saveOrUpdateOvereenkomst(dto);
				}
				else if (object instanceof AanvraagDto dto)
				{
					LOG.info("{} voor type aanvraag(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());
					labformulierService.saveScreenITAanvraag(dto);
				}
				else if (object instanceof WoonplaatsDto dto)
				{
					LOG.info("{} voor type woonplaats(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());
					woonplaatsService.saveScreenITWoonplaats(dto);
				}
				else if (object instanceof ResetDto dto)
				{
					Huisarts huisarts = huisartsService.getHuisartsWith(dto.getHuisarts_id());
					huisarts = authenticatieService.wachtwoordVergeten(huisarts);
					synchronisatieService.syncHuisarts(huisarts);
				}
				else if (object instanceof VerrichtingDto dto)
				{
					LOG.info("{} voor type verrichting(ha_id: {}, s_id: {})", loginfo, dto.getHuisartsportaalId(), dto.getScreenitId());
					verrichtingenService.saveScreenITVerrichting(dto);
				}

				LOG.info("Bericht succesvol verwerkt: {message.getJMSMessageID()}");
			}
		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}
}
