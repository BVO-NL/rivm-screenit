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
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import jakarta.persistence.EntityManager;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.ApplicationEnvironment;
import nl.rivm.screenit.config.CommunicationHubProperties;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtFoutDto;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.communicationhub.api.LetterServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.MessageServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.model.LetterAddress;
import nl.topicuszorg.communicationhub.api.model.LetterDistributor;
import nl.topicuszorg.communicationhub.api.model.LetterFileInput;
import nl.topicuszorg.communicationhub.api.model.LetterMessage;
import nl.topicuszorg.communicationhub.api.model.MessageResponse;
import nl.topicuszorg.communicationhub.api.model.MessageStatus;
import nl.topicuszorg.communicationhub.api.model.NewLetterMessage;
import nl.topicuszorg.communicationhub.api.model.NewMessageRemoval;
import nl.topicuszorg.communicationhub.api.model.ParagonDistributionData;
import nl.topicuszorg.communicationhub.api.model.RemovalType;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.jspecify.annotations.NonNull;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.HttpStatusCodeException;

import com.fasterxml.jackson.core.JsonProcessingException;

@Slf4j
@Configuration
@EnableScheduling
@RequiredArgsConstructor
@Profile("!test")
public class BriefafdrukopdrachtMessageQueueHandler
{

	private final MessageService messageService;

	private final BaseBriefService baseBriefService;

	private final MessageRepository messageRepository;

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final DatabaseRunner databaseRunner;

	private final BatchApplicationType batchApplicationType;

	private final EntityManager entityManager;

	private final LetterServiceCommunicationHubClientApi letterServiceApi;

	private final MessageServiceCommunicationHubClientApi messageServiceApi;

	private final CommunicationHubProperties communicatieHubClientConfig;

	private final String applicationEnvironment;

	private static final int AANTAL_WEKEN_VERWIJDER_PDFS = 2;

	private static final long DEFAULT_MAX_SEND_RETRY_TIME = TimeUnit.MINUTES.toMillis(1);

	private static final long DEFAULT_SEND_RETRY_TIME = TimeUnit.SECONDS.toMillis(3);

	private static final int QUEUE_VERWERK_SIZE = 500;

	private static final int QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD = 500;

	private static final int QUEUE_TE_VERSTUREN_SIZE_THRESHOLD = 1000;

	private long sendRunCounter = 0;

	private long wachtOpCommHubOpdrachtSendBevestigingRunCounter = 0;

	long maxSendRetryTime = DEFAULT_MAX_SEND_RETRY_TIME;

	long sendRetryTime = DEFAULT_SEND_RETRY_TIME;

	private final AtomicBoolean verstuurProblemen = new AtomicBoolean(false);

	private final AtomicBoolean wachtOpCommHubOpOpdrachtSendBevestigingProblemen = new AtomicBoolean(false);

	private boolean queueSizeWarning = false;

	boolean stopVerwerkingVoorTesten = false;

	private final Map<Long, BriefafdrukopdrachtDto> wachtOpCommHubOpdrachtSendBevestigingCache = new ConcurrentHashMap<>(); 

	private enum VerwerkingStatus
	{
		GELUKT,
		VERBINDING_FOUT,
		AFGEKEURD
	}

	@Scheduled(fixedDelay = 10, timeUnit = TimeUnit.SECONDS)
	public void verstuurBriefafdrukopdrachtenNaarParagonViaCommHub()
	{
		try
		{
			verstuurOpdrachten();
			sendRunCounter++;
			if (sendRunCounter % 6 == 0)
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
		do
		{
			var messageMap = new HashMap<Long, BriefafdrukopdrachtDto>();
			fetchMessages(MessageType.BRIEF_AFDRUKKEN, messageMap);
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
					messageStatus.set(verstuurBriefafdrukopdrachtEnSetKlaarVoorWachtenOpCommHubSendBevestiging(message, briefafdrukopdrachtDto));
				});
				if (messageStatus.get() == VerwerkingStatus.VERBINDING_FOUT)
				{
					break;
				}
				logCommunicatieProblemenOpgelost("versturen", verstuurProblemen);
			}
		}
		while (!stopVerwerkingVoorTesten);
	}

	private VerwerkingStatus verstuurBriefafdrukopdrachtEnSetKlaarVoorWachtenOpCommHubSendBevestiging(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var guid = getAndSetGuid(message, briefafdrukopdrachtDto);
		var verwerkingStatus = verstuurBriefafdrukopdrachtMetRetries(briefafdrukopdrachtDto);
		switch (verwerkingStatus)
		{
		case GELUKT:
			LOG.info("Versturen naar CommHub gelukt. Message id: '{}', GUID '{}'", message.getId(), guid);
			var sendMessage = verplaatsMessageNaar(MessageType.BRIEF_AFDRUKKEN_SEND, message, briefafdrukopdrachtDto);
			wachtOpCommHubOpdrachtSendBevestigingCache.put(sendMessage.getId(), briefafdrukopdrachtDto);
			break;

		case AFGEKEURD:
			misluktTeVersturen(message, briefafdrukopdrachtDto, guid, "");
			break;
		}
		return verwerkingStatus;
	}

	private VerwerkingStatus verstuurBriefafdrukopdrachtMetRetries(BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		if (isPatOfKtn())
		{
			return VerwerkingStatus.GELUKT;
		}

		var retry = true;
		var wachtVoorVolgendePoging = false;
		var startSendMessage = System.currentTimeMillis();
		MessageResponse response = null;
		while (retry)
		{
			try
			{
				if (wachtVoorVolgendePoging)
				{
					Thread.sleep(sendRetryTime);
				}
				response = verstuurBriefafdrukopdrachtNaarCommHub(briefafdrukopdrachtDto);
				if (response != null && Boolean.TRUE.equals(response.getSuccess()))
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
				if (System.currentTimeMillis() - startSendMessage > maxSendRetryTime && (response == null || Boolean.FALSE.equals(response.getSuccess())))
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

	private Brief getBrief(BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		return briefafdrukopdrachtDto != null && briefafdrukopdrachtDto.getEntityId() != null ?
			entityManager.find(briefafdrukopdrachtDto.getEntityType(), briefafdrukopdrachtDto.getEntityId()) :
			null;
	}

	private Client getClient(Brief brief)
	{
		if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
		{
			return clientBrief.getClient();
		}
		return null;
	}

	private void logCommunicatieProblemenOpgelost(String methodeName, AtomicBoolean problemen)
	{
		if (problemen.get())
		{
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_VERSTUREN_HERSTELD,
				"Verwerking van het %s van briefafdrukopdrachten is hersteld".formatted(methodeName),
				getBevolkingsonderzoeken());
			problemen.set(false);
		}
	}

	private void logException(Exception exception, String methodeName, AtomicBoolean problemen)
	{
		if (!problemen.get())
		{
			problemen.set(true);
			LOG.error("Fout tijdens het {} van briefafdrukopdrachten.", methodeName, exception);
			var logMessage = "Er is een onbekende fout opgetreden tijdens het %s van briefafdrukopdrachten, neem contact op met Topicus.".formatted(methodeName);
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_BATCH_GESTOPT, logMessage);
		}
		if (exception instanceof InterruptedException)
		{
			Thread.currentThread().interrupt();
		}
	}

	@Scheduled(fixedDelay = 30, timeUnit = TimeUnit.SECONDS)
	public void monitorBriefafdrukopdrachten()
	{
		databaseRunner.runInSessionOnly(() ->
		{
			var queueSizeTeVersturen = messageService.fetchQueueSize(MessageType.BRIEF_AFDRUKKEN, batchApplicationType.name());
			var queueSizeNotYetDequeued = messageService.fetchQueueSize(MessageType.BRIEF_AFDRUKKEN_SEND, batchApplicationType.name());

			logQueueSizeProblemen(queueSizeTeVersturen, queueSizeNotYetDequeued);
		});
	}

	private void logQueueSizeProblemen(Long queueSizeTeVersturen, Long queueSizeNotYetDequeued)
	{
		boolean oldQueueSizeWarningValue = queueSizeWarning;
		queueSizeWarning = queueSizeTeVersturen > QUEUE_TE_VERSTUREN_SIZE_THRESHOLD || queueSizeNotYetDequeued > QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD;
		if (oldQueueSizeWarningValue != queueSizeWarning)
		{
			if (queueSizeWarning)
			{
				LOG.warn("Queue size wordt te groot! Te versturen {}>{}, Nog niet afgemeld {}>{}",
					queueSizeTeVersturen, QUEUE_TE_VERSTUREN_SIZE_THRESHOLD,
					queueSizeNotYetDequeued, QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD);
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_QUEUE_ERG_GROOT,
					"In een van queues staan te veel berichten: Te versturen %s>%s, Nog niet afgemeld %s>%s"
						.formatted(queueSizeTeVersturen, QUEUE_TE_VERSTUREN_SIZE_THRESHOLD,
							queueSizeNotYetDequeued, QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD),
					getBevolkingsonderzoeken());
			}
			else
			{
				LOG.info("Queue size is weer klein genoeg");
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_QUEUE_NORMAAL,
					"Het aantal berichten in de queue is weer normaal.",
					getBevolkingsonderzoeken());
			}
		}
	}

	@Scheduled(fixedDelay = 10, timeUnit = TimeUnit.SECONDS)
	public void wachtOpCommHubOpdrachtenSendBevestiging()
	{
		try
		{
			wachtOpOpdrachtenSendBevestiging();
			wachtOpCommHubOpdrachtSendBevestigingRunCounter++;
			if (wachtOpCommHubOpdrachtSendBevestigingRunCounter % 6 == 0)
			{
				LOG.info("Heartbeat briefafdrukopdrachten dequeue");
			}
		}
		catch (Exception e)
		{
			wachtOpCommHubOpdrachtSendBevestigingCache.clear();
			logException(e, "wachten op de CommHub send bevestiging", wachtOpCommHubOpOpdrachtSendBevestigingProblemen);
		}
	}

	private void wachtOpOpdrachtenSendBevestiging()
	{
		do
		{
			if (wachtOpCommHubOpdrachtSendBevestigingCache.isEmpty())
			{
				fetchMessages(MessageType.BRIEF_AFDRUKKEN_SEND, wachtOpCommHubOpdrachtSendBevestigingCache);
			}
			if (wachtOpCommHubOpdrachtSendBevestigingCache.isEmpty())
			{
				break;
			}
			var messageIds = new ArrayList<>(wachtOpCommHubOpdrachtSendBevestigingCache.keySet());
			for (var messageId : messageIds)
			{
				var verwijderResources = new AtomicBoolean(false);
				var verwijderUitOpdrachtenCache = new AtomicBoolean(false);
				var briefafdrukopdrachtDto = wachtOpCommHubOpdrachtSendBevestigingCache.get(messageId);

				databaseRunner.runInNewTransaction(() ->
				{
					var message = messageRepository.getReferenceById(messageId);
					validateOpdrachtStatus(message, briefafdrukopdrachtDto, verwijderResources, verwijderUitOpdrachtenCache);
				});
				verwijderResources(verwijderResources, briefafdrukopdrachtDto);
				logCommunicatieProblemenOpgelost("wachten op de CommHub send bevestiging", wachtOpCommHubOpOpdrachtSendBevestigingProblemen);
				if (verwijderUitOpdrachtenCache.get())
				{
					wachtOpCommHubOpdrachtSendBevestigingCache.remove(messageId);
				}
			}
		}
		while (!stopVerwerkingVoorTesten);
	}

	private void validateOpdrachtStatus(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto, AtomicBoolean verwijderResources, AtomicBoolean verwijderUitOpdrachtenCache)
	{
		var guid = getAndSetGuid(message, briefafdrukopdrachtDto);
		if (isPatOfKtn())
		{
			verwerkCommHubSendBevestiging(message, briefafdrukopdrachtDto, verwijderResources);
			verwijderUitOpdrachtenCache.set(true);
			return;
		}
		var messageChanges = messageServiceApi.getMessageChanges(communicatieHubClientConfig.getTenant(), guid.toString());
		if (!(messageChanges.getMessage() instanceof LetterMessage letterMessage) || letterMessage.getMessageHistory() == null)
		{
			LOG.warn("Geen message history terug van CommHub; geen send bevestiging kunnen uitlezen. GUID '{}', Kenmerk '{}'", guid, briefafdrukopdrachtDto.getKenmerk());
			return;
		}
		var messageHistory = letterMessage.getMessageHistory();

		var isSendToParagon = messageHistory.stream().anyMatch(historyItem -> MessageStatus.SENT == historyItem.getStatus());
		if (isSendToParagon)
		{
			verwerkCommHubSendBevestiging(message, briefafdrukopdrachtDto, verwijderResources);
			verwijderUitOpdrachtenCache.set(true);
		}
		else
		{
			var errorInHistory = messageHistory.stream().filter(historyItem -> MessageStatus.ERROR == historyItem.getStatus()).findAny().orElse(null);
			if (errorInHistory != null)
			{
				misluktTeVersturen(message, briefafdrukopdrachtDto, guid,
					", Reden: '%s', Foutmelding: '%s'".formatted(errorInHistory.getStatus(), errorInHistory.getErrorFeedback()));
				verwijderUitOpdrachtenCache.set(true);
			}
		}
	}

	private void verwerkCommHubSendBevestiging(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto, AtomicBoolean verwijderResources)
	{
		var guid = briefafdrukopdrachtDto.getGuid();
		LOG.info("Versturen naar Paragon (door CommHub) gelukt. Message id: '{}', GUID: '{}', Kenmerk: '{}'", message.getId(), guid,
			briefafdrukopdrachtDto.getKenmerk());
		messageService.dequeueMessage(message);
		var brief = (Brief) Hibernate.unproxy(getBrief(briefafdrukopdrachtDto));
		if (brief != null)
		{
			brief.setCommHubGuid(guid.toString());
			var verstuurdVoorAfdrukkenOp = currentDateSupplier.getLocalDateTime();
			brief.setVerstuurdVoorAfdrukkenOp(verstuurdVoorAfdrukkenOp);
			if (brief instanceof ProjectBrief projectBrief)
			{
				var clientBrief = projectBrief.getBrief();
				if (clientBrief != null)
				{
					clientBrief.setCommHubGuid(guid.toString());
					clientBrief.setVerstuurdVoorAfdrukkenOp(verstuurdVoorAfdrukkenOp);
				}
			}
			else if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
			{
				var projectBrief = clientBrief.getProjectBrief();
				if (projectBrief != null)
				{
					projectBrief.setCommHubGuid(guid.toString());
					projectBrief.setVerstuurdVoorAfdrukkenOp(verstuurdVoorAfdrukkenOp);
				}
			}
		}
		verwijderResources.set(true);
	}

	private void verwijderResources(AtomicBoolean verwijderResource, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		if (verwijderResource.get() && briefafdrukopdrachtDto != null)
		{
			briefafdrukopdrachtDto.getResources().forEach(baseBriefService::verwijderPdfBestand);
		}
	}

	private void fetchMessages(MessageType messageType, Map<Long, BriefafdrukopdrachtDto> messageMap)
	{
		databaseRunner.runInSessionOnly(() ->
			messageService.fetchMessages(messageType, batchApplicationType.name(), QUEUE_VERWERK_SIZE).forEach(message ->
			{
				try
				{
					messageMap.put(message.getId(), messageService.getContent(message));
				}
				catch (JsonProcessingException e)
				{
					var content = message.getContent();
					LOG.error("Fout bij ophalen van content van briefafdrukopdracht. Message id '{}', Message content: '{}'", message.getId(), content, e);
					var briefAfdrukkenFoutDto = BriefafdrukopdrachtFoutDto.builder()
						.foutiefContentEncoded(content != null ? Base64.getEncoder().encodeToString(content.getBytes(StandardCharsets.UTF_8)) : null)
						.feedback(e.getMessage())
						.guid(getAndSetGuid(message, new BriefafdrukopdrachtDto()))
						.build();
					databaseRunner.runInNewTransaction(() ->
						verplaatsMessageNaar(MessageType.BRIEF_AFDRUKKEN_ERROR, message, briefAfdrukkenFoutDto));
				}
			}));
	}

	private void misluktTeVersturen(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto, UUID guid, String postfixErrorMelding)
	{
		var melding = "Briefafdrukopdracht kon niet verzonden worden. Kenmerk: '%s'".formatted(briefafdrukopdrachtDto.getKenmerk());
		LOG.warn("{}, Message id: '{}', GUID: '{}'{}", melding, message.getId(), guid, postfixErrorMelding);
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_VERSTUREN_MISLUKT, getClient(getBrief(briefafdrukopdrachtDto)), melding,
			getBevolkingsonderzoeken());
		verplaatsMessageNaar(MessageType.BRIEF_AFDRUKKEN_ERROR, message, briefafdrukopdrachtDto);
	}

	private static @NonNull UUID getAndSetGuid(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var guid = briefafdrukopdrachtDto.getGuid() != null ?
			briefafdrukopdrachtDto.getGuid() :
			UUID.nameUUIDFromBytes(message.getId().toString().getBytes(StandardCharsets.UTF_8));
		briefafdrukopdrachtDto.setGuid(guid);
		return guid;
	}

	private Message verplaatsMessageNaar(MessageType nieuwQueueType, Message oudeMessage, Serializable contentDto)
	{
		messageService.dequeueMessage(oudeMessage);
		return messageService.queueMessage(nieuwQueueType, contentDto, batchApplicationType.name());
	}

	private Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return batchApplicationType == BatchApplicationType.GENERALIS ?
			new Bevolkingsonderzoek[] {} :
			new Bevolkingsonderzoek[] { Bevolkingsonderzoek.valueOf(batchApplicationType.name()) };
	}

	private boolean isPatOfKtn()
	{
		return ApplicationEnvironment.PAT.getEnvNaam().equalsIgnoreCase(applicationEnvironment)
			|| ApplicationEnvironment.OPL.getEnvNaam().equalsIgnoreCase(applicationEnvironment);
	}
}
