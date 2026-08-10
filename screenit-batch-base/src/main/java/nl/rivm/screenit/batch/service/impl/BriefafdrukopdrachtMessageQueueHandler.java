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

import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import jakarta.persistence.EntityManager;

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
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;

import org.jspecify.annotations.NonNull;

import com.fasterxml.jackson.core.JsonProcessingException;

@Slf4j
public abstract class BriefafdrukopdrachtMessageQueueHandler
{

	private static final long MIN_POLL_JITTER_MS = 100;

	private static final long MAX_POLL_JITTER_MS = 300;

	private static final long POLL_JITTER_RANGE_MS = (MAX_POLL_JITTER_MS - MIN_POLL_JITTER_MS) + 1;

	protected final MessageService messageService;

	protected final BaseBriefService baseBriefService;

	protected final MessageRepository messageRepository;

	protected final LogService logService;

	protected final ICurrentDateSupplier currentDateSupplier;

	protected final DatabaseRunner databaseRunner;

	protected final BatchApplicationType batchApplicationType;

	protected final EntityManager entityManager;

	protected final CommunicationHubProperties communicatieHubClientConfig;

	protected final BriefafdrukopdrachtMessageQueueStateService queueStateService;

	protected final String applicationEnvironment;

	protected BriefafdrukopdrachtMessageQueueHandler(MessageService messageService, BaseBriefService baseBriefService, MessageRepository messageRepository, LogService logService,
		ICurrentDateSupplier currentDateSupplier, DatabaseRunner databaseRunner, BatchApplicationType batchApplicationType, EntityManager entityManager,
		CommunicationHubProperties communicatieHubClientConfig,
		BriefafdrukopdrachtMessageQueueStateService queueStateService, String applicationEnvironment)
	{
		this.messageService = messageService;
		this.baseBriefService = baseBriefService;
		this.messageRepository = messageRepository;
		this.logService = logService;
		this.currentDateSupplier = currentDateSupplier;
		this.databaseRunner = databaseRunner;
		this.batchApplicationType = batchApplicationType;
		this.entityManager = entityManager;
		this.communicatieHubClientConfig = communicatieHubClientConfig;
		this.queueStateService = queueStateService;
		this.applicationEnvironment = applicationEnvironment;
	}

	protected void voegMessageToeAanMap(Map<Long, BriefafdrukopdrachtSendBevestigingPollStatus> messageMap, Message message)
	{
		var messageId = message.getId();
		try
		{

			BriefafdrukopdrachtDto dto = messageService.getContent(message);
			zetBerichtInMessageMap(messageMap, message, dto);
		}
		catch (JsonProcessingException e)
		{
			var content = message.getContent();
			LOG.error("Fout bij ophalen van content van briefafdrukopdracht. Message id '{}', Message content: '{}'", messageId, content, e);
			var briefAfdrukkenFoutDto = BriefafdrukopdrachtFoutDto.builder()
				.foutiefContentEncoded(content != null ? Base64.getEncoder().encodeToString(content.getBytes(StandardCharsets.UTF_8)) : null)
				.feedback(e.getMessage())
				.guid(getAndSetGuid(message, new BriefafdrukopdrachtDto()))
				.build();
			databaseRunner.runInNewTransaction(() ->
			{
				var attachedMessage = messageRepository.getReferenceById(messageId);
				verplaatsMessageNaar(attachedMessage, MessageType.BRIEF_AFDRUKKEN_ERROR, briefAfdrukkenFoutDto);
			});
		}
	}

	protected void zetBerichtInMessageMap(Map<Long, BriefafdrukopdrachtSendBevestigingPollStatus> messageMap, Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		messageMap.putIfAbsent(message.getId(),
			new BriefafdrukopdrachtSendBevestigingPollStatus(briefafdrukopdrachtDto, System.currentTimeMillis() + berekenJitterMillis(message.getId()), 0));
	}

	protected long berekenJitterMillis(Long messageId)
	{
		return MIN_POLL_JITTER_MS + Math.floorMod(messageId, POLL_JITTER_RANGE_MS);
	}

	protected void misluktTeVersturen(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto, UUID guid, String postfixErrorMelding)
	{
		var melding = "Briefafdrukopdracht kon niet verzonden worden. Kenmerk: '%s'".formatted(briefafdrukopdrachtDto.getKenmerk());
		LOG.warn("{}, Message id: '{}', GUID: '{}'{}", melding, message.getId(), guid, postfixErrorMelding);
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_VERSTUREN_MISLUKT, getClient(getBrief(briefafdrukopdrachtDto)), melding,
			getBevolkingsonderzoeken());
		verplaatsMessageNaar(message, MessageType.BRIEF_AFDRUKKEN_ERROR, briefafdrukopdrachtDto);
	}

	protected Brief getBrief(BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		return briefafdrukopdrachtDto != null && briefafdrukopdrachtDto.getEntityId() != null ?
			entityManager.find(briefafdrukopdrachtDto.getEntityType(), briefafdrukopdrachtDto.getEntityId()) :
			null;
	}

	protected Client getClient(Brief brief)
	{
		if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
		{
			return clientBrief.getClient();
		}
		return null;
	}

	protected void logException(Exception exception, String methodeName, AtomicBoolean problemen)
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

	protected void logCommunicatieProblemenOpgelost(String methodeName, AtomicBoolean problemen)
	{
		if (problemen.get())
		{
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_VERSTUREN_HERSTELD,
				"Verwerking van het %s van briefafdrukopdrachten is hersteld".formatted(methodeName),
				getBevolkingsonderzoeken());
			problemen.set(false);
		}
	}

	protected static @NonNull UUID getAndSetGuid(Message message, BriefafdrukopdrachtDto briefafdrukopdrachtDto)
	{
		var guid = briefafdrukopdrachtDto.getGuid() != null ?
			briefafdrukopdrachtDto.getGuid() :
			UUID.nameUUIDFromBytes(message.getId().toString().getBytes(StandardCharsets.UTF_8));
		briefafdrukopdrachtDto.setGuid(guid);
		return guid;
	}

	protected void verplaatsMessageNaar(Message message, MessageType nieuwQueueType, Serializable contentDto)
	{
		message.setType(nieuwQueueType);
		messageService.updateMessageContent(message, contentDto);
	}

	protected Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return batchApplicationType == BatchApplicationType.GENERALIS ?
			new Bevolkingsonderzoek[] {} :
			new Bevolkingsonderzoek[] { Bevolkingsonderzoek.valueOf(batchApplicationType.name()) };
	}

	protected boolean isPatOfOpl()
	{
		return ApplicationEnvironment.PAT.getEnvNaam().equalsIgnoreCase(applicationEnvironment)
			|| ApplicationEnvironment.OPL.getEnvNaam().equalsIgnoreCase(applicationEnvironment);
	}
}
