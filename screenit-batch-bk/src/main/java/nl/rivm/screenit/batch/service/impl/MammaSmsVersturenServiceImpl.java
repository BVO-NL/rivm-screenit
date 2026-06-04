package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.time.Instant;
import java.util.AbstractMap;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.ApplicationEnvironment;
import nl.rivm.screenit.config.CommunicationHubClientConfig;
import nl.rivm.screenit.dto.SmsVersturenDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.mamma.MammaBaseAfspraakRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.topicuszorg.communicationhub.api.SmsServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.model.NewSmsMessage;
import nl.topicuszorg.communicationhub.api.model.Receiver;
import nl.topicuszorg.communicationhub.api.model.SmsReceiver;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
@EnableScheduling
@RequiredArgsConstructor
public class MammaSmsVersturenServiceImpl
{
	private static final String RECEIVER_REFERENCE_TYPE = "afspraakId";

	private final MammaDigitaalContactService digitaalContactService;

	private final SmsServiceCommunicationHubClientApi smsServiceApi;

	private final MammaBaseAfspraakRepository afspraakRepository;

	private final CommunicationHubClientConfig communicationHubClientConfig;

	private final LogService logService;

	private final String applicationEnvironment;

	@Scheduled(cron = "0 */1 8-21 * * *")
	public void verstuurSmsBerichtenNaarCommHub()
	{
		LOG.info("Running sms berichten herinnering");

		var afsprakenIds = digitaalContactService.getAfsprakenVoorSmsVersturen();

		var afspraakIdGuidMap = afsprakenIds.stream()
			.flatMap(afspraakId ->
			{
				try
				{
					return digitaalContactService.maakSmsVersturenDTO(afspraakId)
						.stream()
						.map(dto ->
						{
							var client = bepaalClient(afspraakId);
							var newSmsMessage = mapNaarNewSmsMessage(dto, afspraakId);

							String guid;
							if (isPatOfOplOmgeving())
							{
								guid = UUID.randomUUID().toString();
							}
							else
							{
								var response = smsServiceApi.newSms(communicationHubClientConfig.getTenant(), newSmsMessage);
								guid = response.getGuid();
							}
							logSmsVerzonden(newSmsMessage, client);

							return new AbstractMap.SimpleEntry<>(afspraakId, guid);
						})
						.toList()
						.stream();
				}
				catch (Exception e)
				{
					LOG.error("SMS versturen mislukt voor afspraakId {}", afspraakId, e);
					return Stream.<Map.Entry<Long, String>> empty();
				}
			})
			.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

		digitaalContactService.administreerSmsVerstuurd(afspraakIdGuidMap);
		LOG.info("Aantal: {} verwerkt, Totaal: {} verstuurd", afsprakenIds.size(), afspraakIdGuidMap.size());
	}

	private NewSmsMessage mapNaarNewSmsMessage(SmsVersturenDto smsDto, Long afspraakId)
	{
		var receiver = new Receiver();
		receiver.setReference(String.valueOf(afspraakId));
		receiver.setReferenceType(RECEIVER_REFERENCE_TYPE);

		var smsReceiver = new SmsReceiver();
		smsReceiver.setReceiver(receiver);
		smsReceiver.setPhoneNumber(smsDto.getTelefoonnummer());

		var newSmsMessage = new NewSmsMessage();
		newSmsMessage.setSmsReceiver(smsReceiver);
		newSmsMessage.setContent(smsDto.getTekst());
		newSmsMessage.setOriginator(communicationHubClientConfig.getSmsSender());
		newSmsMessage.setSendAt(Instant.now());
		return newSmsMessage;
	}

	private void logSmsVerzonden(NewSmsMessage newSmsMessage, Client client)
	{
		var ontvangendTelefoonNummer = newSmsMessage.getSmsReceiver().getPhoneNumber();

		logService.logGebeurtenis(
			LogGebeurtenis.MAMMA_DIGITAAL_CONTACT_SMS_VERZONDEN,
			client,
			"BK afspraak herinnering SMS verzonden naar " + ontvangendTelefoonNummer,
			Bevolkingsonderzoek.MAMMA
		);
	}

	private Client bepaalClient(Long afspraakId)
	{
		return afspraakRepository.findClientByAfspraakId(afspraakId)
			.orElseThrow(() -> new IllegalStateException("Kan geen client vinden voor afspraakId: " + afspraakId));
	}

	private boolean isPatOfOplOmgeving()
	{
		return ApplicationEnvironment.PAT.getEnvNaam().equalsIgnoreCase(applicationEnvironment)
			|| ApplicationEnvironment.OPL.getEnvNaam().equalsIgnoreCase(applicationEnvironment);
	}
}
