package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.IOException;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import jakarta.persistence.EntityManager;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.CommunicationHubProperties;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.communicationhub.api.LetterServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.model.LetterAddress;
import nl.topicuszorg.communicationhub.api.model.LetterDistributor;
import nl.topicuszorg.communicationhub.api.model.LetterFileInput;
import nl.topicuszorg.communicationhub.api.model.MessageResponse;
import nl.topicuszorg.communicationhub.api.model.NewLetterMessage;
import nl.topicuszorg.communicationhub.api.model.NewMessageRemoval;
import nl.topicuszorg.communicationhub.api.model.ParagonDistributionData;
import nl.topicuszorg.communicationhub.api.model.RemovalType;

import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.HttpStatusCodeException;

@Slf4j
@Configuration
@EnableScheduling
@Profile("!test")
public class BriefafdrukopdrachtVersturenScheduler extends BriefafdrukopdrachtMessageQueueHandler
{
	private static final int AANTAL_WEKEN_VERWIJDER_PDFS = 2;

	private static final long DEFAULT_MAX_SEND_RETRY_TIME = TimeUnit.MINUTES.toMillis(1);

	private static final long DEFAULT_SEND_RETRY_TIME = TimeUnit.SECONDS.toMillis(3);

	private static final int QUEUE_VERWERK_SIZE = 500;

	private final LetterServiceCommunicationHubClientApi letterServiceApi;

	private final AtomicBoolean verstuurProblemen = new AtomicBoolean(false);

	private long verstuurRunCounter;

	long maxSendRetryTime = DEFAULT_MAX_SEND_RETRY_TIME;

	long sendRetryTime = DEFAULT_SEND_RETRY_TIME;

	private enum VerwerkingStatus
	{
		GELUKT,
		VERBINDING_FOUT,
		AFGEKEURD
	}

	public BriefafdrukopdrachtVersturenScheduler(MessageService messageService, BaseBriefService baseBriefService, MessageRepository messageRepository, LogService logService,
		ICurrentDateSupplier currentDateSupplier, DatabaseRunner databaseRunner, BatchApplicationType batchApplicationType, EntityManager entityManager,
		CommunicationHubProperties communicatieHubClientConfig,
		BriefafdrukopdrachtMessageQueueStateService queueStateService, String applicationEnvironment,
		LetterServiceCommunicationHubClientApi letterServiceApi)
	{
		super(messageService, baseBriefService, messageRepository, logService, currentDateSupplier, databaseRunner, batchApplicationType, entityManager,
			communicatieHubClientConfig, queueStateService, applicationEnvironment);
		this.letterServiceApi = letterServiceApi;
	}

	@Scheduled(fixedDelayString = "${commhub.briefafdrukopdracht-versturen.scheduler-delay-ms:10000}")
	public void verstuurBriefafdrukopdrachtenNaarParagonViaCommHub()
	{
		try
		{
			verstuurOpdrachten();
			verstuurRunCounter++;
			if (verstuurRunCounter % 6 == 0)
			{
				LOG.info("Heartbeat briefafdrukopdrachten versturen queue");
			}
		}
		catch (Exception e)
		{
			logException(e, "versturen", verstuurProblemen);
		}
	}

	private void verstuurOpdrachten()
	{
		while (true)
		{
			var messageMap = new HashMap<Long, BriefafdrukopdrachtSendBevestigingPollStatus>();
			fetchMessages(messageMap);
			if (messageMap.isEmpty())
			{
				break;
			}
			for (var messageEntry : messageMap.entrySet())
			{
				var briefafdrukopdrachtDto = messageEntry.getValue();
				var messageStatus = new AtomicReference<VerwerkingStatus>();
				databaseRunner.runInNewTransaction(() ->
				{
					var message = messageRepository.getReferenceById(messageEntry.getKey());
					messageStatus.set(verstuurBriefafdrukopdrachtEnSetKlaarVoorWachtenOpCommHubSendBevestiging(message, briefafdrukopdrachtDto.briefafdrukopdrachtDto));
				});
				if (messageStatus.get() == VerwerkingStatus.VERBINDING_FOUT)
				{
					return;
				}
				logCommunicatieProblemenOpgelost("versturen", verstuurProblemen);
			}
		}
	}

	private VerwerkingStatus verstuurBriefafdrukopdrachtEnSetKlaarVoorWachtenOpCommHubSendBevestiging(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var guid = getAndSetGuid(message, briefafdrukopdrachtDto);
		var verwerkingStatus = VerwerkingStatus.GELUKT;
		if (!isPatOfOpl())
		{
			verwerkingStatus = verstuurBriefafdrukopdrachtMetRetries(briefafdrukopdrachtDto);
		}
		switch (verwerkingStatus)
		{
		case GELUKT:
			LOG.info("Versturen naar CommHub gelukt. Message id: '{}', GUID '{}'", message.getId(), guid);
			verplaatsMessageNaar(message, MessageType.BRIEF_AFDRUKKEN_SEND, briefafdrukopdrachtDto);
			if (!queueStateService.isSendBevestigingCacheVol())
			{
				zetBerichtInMessageMap(queueStateService.getWachtOpCommHubOpdrachtSendBevestigingCache(), message, briefafdrukopdrachtDto);
			}
			break;
		case AFGEKEURD:
			misluktTeVersturen(message, briefafdrukopdrachtDto, guid, "");
			break;
		case VERBINDING_FOUT:
			break;
		}
		return verwerkingStatus;
	}

	private VerwerkingStatus verstuurBriefafdrukopdrachtMetRetries(BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var retry = true;
		var wachtVoorVolgendePoging = false;
		var startSendMessage = System.currentTimeMillis();
		MessageResponse response;

		while (retry)
		{
			var succesvolResponse = false;
			try
			{
				if (wachtVoorVolgendePoging)
				{
					Thread.sleep(sendRetryTime);
				}
				response = verstuurBriefafdrukopdrachtNaarCommHub(briefafdrukopdrachtDto);
				succesvolResponse = response != null && Boolean.TRUE.equals(response.getSuccess());
				if (succesvolResponse)
				{
					return VerwerkingStatus.GELUKT;
				}
				LOG.error("Fout bij versturen van briefafdrukopdracht naar CommHub. GUID: '{}', Kenmerk: '{}', Response: '{}'", briefafdrukopdrachtDto.getGuid(),
					briefafdrukopdrachtDto.getKenmerk(), response);
				return VerwerkingStatus.AFGEKEURD;
			}
			catch (InterruptedException e)
			{
				Thread.currentThread().interrupt();
				return VerwerkingStatus.VERBINDING_FOUT;
			}
			catch (Exception e)
			{
				var status = handleException(e, briefafdrukopdrachtDto);
				if (status != VerwerkingStatus.VERBINDING_FOUT)
				{
					return status;
				}
			}
			finally
			{
				wachtVoorVolgendePoging = true;
				var langdurigGeenResponse = System.currentTimeMillis() - startSendMessage > maxSendRetryTime;
				if (langdurigGeenResponse && !succesvolResponse)
				{
					LOG.error("Lukt niet om bericht te versturen. Stop met retry.");
					retry = false;
				}
			}
		}

		return VerwerkingStatus.VERBINDING_FOUT;
	}

	private MessageResponse verstuurBriefafdrukopdrachtNaarCommHub(BriefafdrukopdrachtDto briefafdrukopdrachtDto) throws IOException
	{
		var letterFileInputs = new LinkedHashSet<LetterFileInput>();
		for (var resource : briefafdrukopdrachtDto.getResources())
		{
			try (var fileStream = baseBriefService.getFileStreamVanPdfBestand(resource))
			{
				var base64Content = Base64.getEncoder().encodeToString(fileStream.readAllBytes());

				letterFileInputs.add(LetterFileInput.builder()
					.sequenceNumber(resource.getOrder())
					.base64Content(base64Content).fileName(resource.getPath()).mimeType("application/pdf")
					.build());
			}
		}

		var newLetterMessage = NewLetterMessage.builder()
			.address(LetterAddress.builder().lastName("dummy").firstName("dummy").street("dummy").houseNumber("0").postalCode("1111AA").city("dummy").countryCode("NL")
				.build()) 
			.files(letterFileInputs)
			.distributionData(ParagonDistributionData.builder()
				.distributor(LetterDistributor.PARAGON)
				.orderType(briefafdrukopdrachtDto.getCode())
				.orderReference(briefafdrukopdrachtDto.getKenmerk())
				.orderReferenceExtra(briefafdrukopdrachtDto.getTimestamp())
				.deliveryReference(StringUtils.defaultIfBlank(briefafdrukopdrachtDto.getCodeAddendum(), null))
				.build())
			.messageRemovals(
				List.of(NewMessageRemoval.builder().removalType(RemovalType.CONTENT).plannedRemovalDate(LocalDate.now().plusWeeks(AANTAL_WEKEN_VERWIJDER_PDFS)).build()))
			.sendAt(ZonedDateTime.now(DateUtil.SCREENIT_DEFAULT_ZONE).toInstant())
			.build();

		return letterServiceApi.upsertLetter(communicatieHubClientConfig.getTenant(), briefafdrukopdrachtDto.getGuid().toString(), newLetterMessage);
	}

	private void fetchMessages(Map<Long, BriefafdrukopdrachtSendBevestigingPollStatus> messageMap)
	{
		databaseRunner.runInSessionOnly(() ->
			messageService.fetchMessages(MessageType.BRIEF_AFDRUKKEN, batchApplicationType.name(), QUEUE_VERWERK_SIZE).forEach(message ->
			{
				voegMessageToeAanMap(messageMap, message);
			}));
	}

	private VerwerkingStatus handleException(Exception exception, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var postFix = "GUID: '%s', Kenmerk: '%s'".formatted(briefafdrukopdrachtDto.getGuid(), briefafdrukopdrachtDto.getKenmerk());
		if (exception instanceof HttpStatusCodeException httpStatusCodeException)
		{
			if (HttpStatus.CONFLICT == httpStatusCodeException.getStatusCode())
			{
				LOG.warn("Krijgen een conflict (2 keer zelfde GUID, resend) nav briefafdrukopdracht naar CommHub. {}", postFix);
				return VerwerkingStatus.GELUKT;
			}
			else if (HttpStatus.INTERNAL_SERVER_ERROR == httpStatusCodeException.getStatusCode())
			{
				LOG.error("Inhoudelijke fout bij versturen van briefafdrukopdracht naar CommHub. {}, Response: '{}'", postFix,
					httpStatusCodeException.getResponseBodyAsString(), exception);
				return VerwerkingStatus.AFGEKEURD;
			}
			else
			{
				LOG.error("Technische/connectie fout bij versturen van briefafdrukopdracht naar CommHub. {}, Response: '{}'", postFix,
					httpStatusCodeException.getResponseBodyAsString(), exception);
				return VerwerkingStatus.VERBINDING_FOUT;
			}
		}
		else
		{
			LOG.error("Fout bij versturen van briefafdrukopdracht naar CommHub. {}", postFix, exception);
			return VerwerkingStatus.VERBINDING_FOUT;
		}
	}
}
