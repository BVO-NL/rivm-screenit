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

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import jakarta.persistence.EntityManager;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.CommunicationHubProperties;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.topicuszorg.communicationhub.api.MessageServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.model.LetterMessage;
import nl.topicuszorg.communicationhub.api.model.MessageChangesResponse;
import nl.topicuszorg.communicationhub.api.model.MessageStatus;

import org.hibernate.Hibernate;
import org.jspecify.annotations.NonNull;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
@EnableScheduling
@Profile("!test")
public class BriefafdrukopdrachtSendBevestigingScheduler extends BriefafdrukopdrachtMessageQueueHandler
{
	private static final int MAX_SEND_BEVESTIGING_CACHE_BIJVUL_BATCHES = 3;

	private static final long SEND_BEVESTIGING_CACHE_BIJVUL_INTERVAL_MS = TimeUnit.SECONDS.toMillis(30);

	private final MessageServiceCommunicationHubClientApi messageServiceApi;

	private final AtomicBoolean wachtOpSendBevestigingProblemen = new AtomicBoolean(false);

	private long wachtOpSendBevestigingRunCounter;

	private enum SendBevestigingPollingResult
	{
		AFGEHANDELD,
		GEEN_STATUS,
		TECHNISCHE_FOUT
	}

	@RequiredArgsConstructor
	private static class SendBevestigingRunStatus
	{
		private final long runStart;

		private final int maxPollsPerRun;

		private final long maxRunDuration;

		private int pollsInDezeRun;

		private boolean voortgangGeboekt;

		private boolean technischeFoutOpgetreden;

		private boolean hadPendingBerichten;

		int resterendPollBudget()
		{
			return maxPollsPerRun - pollsInDezeRun;
		}
	}

	public BriefafdrukopdrachtSendBevestigingScheduler(MessageService messageService, BaseBriefService baseBriefService, MessageRepository messageRepository, LogService logService,
		ICurrentDateSupplier currentDateSupplier, DatabaseRunner databaseRunner, BatchApplicationType batchApplicationType, EntityManager entityManager,
		CommunicationHubProperties communicatieHubClientConfig,
		BriefafdrukopdrachtMessageQueueStateService queueStateService, String applicationEnvironment,
		MessageServiceCommunicationHubClientApi messageServiceApi)
	{
		super(messageService, baseBriefService, messageRepository, logService, currentDateSupplier, databaseRunner, batchApplicationType, entityManager,
			communicatieHubClientConfig, queueStateService, applicationEnvironment);
		this.messageServiceApi = messageServiceApi;
	}

	@Scheduled(fixedDelayString = "${commhub.briefafdrukopdracht-send-bevestiging.scheduler-delay-ms:10000}")
	public void wachtOpCommHubOpdrachtenSendBevestiging()
	{
		try
		{
			wachtOpOpdrachtenSendBevestiging();
			wachtOpSendBevestigingRunCounter++;
			if (wachtOpSendBevestigingRunCounter % 6 == 0)
			{
				LOG.info("Heartbeat briefafdrukopdrachten dequeue");
			}
		}
		catch (Exception e)
		{
			queueStateService.verhoogOpeenvolgendeSendBevestigingRunsZonderVoortgang();
			queueStateService.verhoogOpeenvolgendeSendBevestigingTechnischeFouten();
			logException(e, "wachten op de CommHub send bevestiging", wachtOpSendBevestigingProblemen);
		}
	}

	private void wachtOpOpdrachtenSendBevestiging()
	{
		var runStatus = maakSendBevestigingRunStatus();
		while (isRunActief(runStatus))
		{
			if (!vulSendBevestigingCacheAlsNodig(runStatus))
			{
				break;
			}
			var tePollenBerichten = bepaalTePollenBerichten(System.currentTimeMillis(), runStatus.resterendPollBudget());
			if (tePollenBerichten.isEmpty())
			{
				break;
			}
			verwerkTePollenBerichten(runStatus, tePollenBerichten);
		}
		verwerkSendBevestigingRunResultaat(runStatus);
	}

	private SendBevestigingRunStatus maakSendBevestigingRunStatus()
	{
		var sendBevestigingProperties = communicatieHubClientConfig.getBriefafdrukopdrachtSendBevestiging();
		return new SendBevestigingRunStatus(System.currentTimeMillis(), sendBevestigingProperties.getMaxPollsPerRun(),
			sendBevestigingProperties.getMaxRunDurationMs());
	}

	private boolean isRunActief(SendBevestigingRunStatus runStatus)
	{
		return runStatus.pollsInDezeRun < runStatus.maxPollsPerRun
			&& System.currentTimeMillis() - runStatus.runStart < runStatus.maxRunDuration;
	}

	private boolean vulSendBevestigingCacheAlsNodig(SendBevestigingRunStatus runStatus)
	{
		if (moetSendBevestigingCacheBijgevuldWorden())
		{
			vulSendBevestigingCachePeriodiekAan();
		}
		if (queueStateService.isSendBevestigingCacheLeeg())
		{
			return false;
		}
		runStatus.hadPendingBerichten = true;
		return true;
	}

	private boolean moetSendBevestigingCacheBijgevuldWorden()
	{
		if (queueStateService.isSendBevestigingCacheVol())
		{
			return false;
		}
		if (queueStateService.isSendBevestigingCacheLeeg())
		{
			return true;
		}
		return System.currentTimeMillis() - queueStateService.getLaatsteSendBevestigingCacheBijvulMoment() >= SEND_BEVESTIGING_CACHE_BIJVUL_INTERVAL_MS;
	}

	private void vulSendBevestigingCachePeriodiekAan()
	{
		queueStateService.setLaatsteSendBevestigingCacheBijvulMoment(System.currentTimeMillis());
		var batch = 0;
		while (batch < MAX_SEND_BEVESTIGING_CACHE_BIJVUL_BATCHES && !queueStateService.isSendBevestigingCacheVol())
		{

			var vrijePlekken = queueStateService.aantalVrijePlekkenInSendBevestigingCache();
			var opgehaaldeBerichten = fetchMessages(queueStateService.getSendBevestigingFetchCursorMessageId(), vrijePlekken);
			if (opgehaaldeBerichten == 0)
			{
				break;
			}
			batch++;
		}
	}

	private void verwerkTePollenBerichten(SendBevestigingRunStatus runStatus, List<Map.Entry<Long, BriefafdrukopdrachtSendBevestigingPollStatus>> tePollenBerichten)
	{
		for (var berichtEntry : tePollenBerichten)
		{
			if (!isRunActief(runStatus))
			{
				break;
			}
			verwerkTePollenBericht(runStatus, berichtEntry);
		}
	}

	private void verwerkTePollenBericht(SendBevestigingRunStatus runStatus, Map.Entry<Long, BriefafdrukopdrachtSendBevestigingPollStatus> berichtEntry)
	{
		var messageId = berichtEntry.getKey();
		var pollStatus = berichtEntry.getValue();
		runStatus.pollsInDezeRun++;
		var result = verwerkSendBevestigingPoll(messageId, pollStatus);
		switch (result)
		{
		case AFGEHANDELD:
			queueStateService.getWachtOpCommHubOpdrachtSendBevestigingCache().remove(messageId);
			runStatus.voortgangGeboekt = true;
			break;
		case TECHNISCHE_FOUT:
			runStatus.technischeFoutOpgetreden = true;
			planVolgendePoll(pollStatus, true);
			break;
		case GEEN_STATUS:
			planVolgendePoll(pollStatus, false);
			break;
		}
	}

	private List<Map.Entry<Long, BriefafdrukopdrachtSendBevestigingPollStatus>> bepaalTePollenBerichten(long nu, int maxAantal)
	{
		return queueStateService.getWachtOpCommHubOpdrachtSendBevestigingCache().entrySet().stream()
			.filter(entry -> entry.getValue().volgendePollVanaf <= nu)
			.sorted(
				Comparator.<Map.Entry<Long, BriefafdrukopdrachtSendBevestigingPollStatus>, Long> comparing(entry -> entry.getValue().volgendePollVanaf)
					.thenComparing(Map.Entry::getKey))
			.limit(maxAantal)
			.toList();
	}

	private SendBevestigingPollingResult verwerkSendBevestigingPoll(Long messageId, BriefafdrukopdrachtSendBevestigingPollStatus pollStatus)
	{
		var verwijderResources = new AtomicBoolean(false);
		var result = new AtomicReference<SendBevestigingPollingResult>();
		try
		{
			databaseRunner.runInNewTransaction(() ->
			{
				var message = messageRepository.getReferenceById(messageId);
				result.set(valideerOpdrachtStatus(message, pollStatus, verwijderResources));
			});
			verwijderResources(verwijderResources, pollStatus.briefafdrukopdrachtDto);
			return result.get() != null ? result.get() : SendBevestigingPollingResult.TECHNISCHE_FOUT;
		}
		catch (Exception e)
		{
			LOG.warn("Technische fout tijdens ophalen van send bevestiging. Message id '{}', Kenmerk '{}'", messageId,
				pollStatus.briefafdrukopdrachtDto != null ? pollStatus.briefafdrukopdrachtDto.getKenmerk() : null, e);
			return SendBevestigingPollingResult.TECHNISCHE_FOUT;
		}
	}

	private SendBevestigingPollingResult valideerOpdrachtStatus(Message message, BriefafdrukopdrachtSendBevestigingPollStatus pollStatus, AtomicBoolean verwijderResources)
	{
		var briefafdrukopdrachtDto = pollStatus.briefafdrukopdrachtDto;
		var guid = getAndSetGuid(message, briefafdrukopdrachtDto);
		if (isPatOfOpl())
		{
			verwerkCommHubSendBevestiging(message, pollStatus, verwijderResources);
			return SendBevestigingPollingResult.AFGEHANDELD;
		}
		MessageChangesResponse messageChanges;
		try
		{
			messageChanges = messageServiceApi.getMessageChanges(communicatieHubClientConfig.getTenant(), guid.toString());
		}
		catch (Exception e)
		{
			LOG.warn("Geen send bevestiging kunnen ophalen door technische fout. GUID '{}', Kenmerk '{}'", guid, briefafdrukopdrachtDto.getKenmerk(), e);
			return SendBevestigingPollingResult.TECHNISCHE_FOUT;
		}

		if (messageChanges == null || !(messageChanges.getMessage() instanceof LetterMessage letterMessage) || letterMessage.getMessageHistory() == null)
		{
			LOG.warn("Geen message history terug van CommHub; geen send bevestiging kunnen uitlezen. GUID '{}', Kenmerk '{}'", guid, briefafdrukopdrachtDto.getKenmerk());
			return SendBevestigingPollingResult.GEEN_STATUS;
		}
		var messageHistory = letterMessage.getMessageHistory();

		var isSendToParagon = messageHistory.stream().anyMatch(historyItem -> MessageStatus.SENT == historyItem.getStatus());
		if (isSendToParagon)
		{
			verwerkCommHubSendBevestiging(message, pollStatus, verwijderResources);
			return SendBevestigingPollingResult.AFGEHANDELD;
		}

		var errorInHistory = messageHistory.stream().filter(historyItem -> MessageStatus.ERROR == historyItem.getStatus()).findAny().orElse(null);
		if (errorInHistory != null)
		{
			misluktTeVersturen(message, briefafdrukopdrachtDto, guid,
				", Reden: '%s', Foutmelding: '%s'".formatted(errorInHistory.getStatus(), errorInHistory.getErrorFeedback()));
			return SendBevestigingPollingResult.AFGEHANDELD;
		}
		return SendBevestigingPollingResult.GEEN_STATUS;
	}

	private void verwerkCommHubSendBevestiging(Message message, BriefafdrukopdrachtSendBevestigingPollStatus pollStatus, AtomicBoolean verwijderResources)
	{
		var briefafdrukopdrachtDto = pollStatus.briefafdrukopdrachtDto;
		var guid = briefafdrukopdrachtDto.getGuid();
		messageService.dequeueMessage(message);
		LOG.info("Versturen naar Paragon (door CommHub) gelukt. Message id: '{}', GUID: '{}', Kenmerk: '{}', Pollpogingen: {}", message.getId(),
			briefafdrukopdrachtDto.getGuid(), briefafdrukopdrachtDto.getKenmerk(), pollStatus.aantalPollPogingen);
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

	private int fetchMessages(long vanafMessageIdExclusief, int maxFetchSize)
	{
		var fetchedCount = new AtomicReference<>(0);
		if (maxFetchSize > 0)
		{
			databaseRunner.runInSessionOnly(() ->
			{
				var berichten = messageService.fetchMessagesGroterDanId(MessageType.BRIEF_AFDRUKKEN_SEND, batchApplicationType.name(), vanafMessageIdExclusief, maxFetchSize);
				berichten.forEach(message ->
				{
					voegMessageToeAanMap(queueStateService.getWachtOpCommHubOpdrachtSendBevestigingCache(), message);
					queueStateService.updateSendBevestigingFetchCursorMessageId(message.getId());
				});
				fetchedCount.set(berichten.size());
			});
		}
		return fetchedCount.get();
	}

	private void planVolgendePoll(BriefafdrukopdrachtSendBevestigingPollStatus pollStatus, boolean technischeFout)
	{
		pollStatus.aantalPollPogingen++;
		var interval = berekenVolgendPollInterval(
			pollStatus.aantalPollPogingen,
			technischeFout,
			queueStateService.getOpeenvolgendeSendBevestigingRunsZonderVoortgang(),
			queueStateService.getOpeenvolgendeSendBevestigingTechnischeFouten());
		pollStatus.volgendePollVanaf = System.currentTimeMillis() + interval;
	}

	private long berekenVolgendPollInterval(int aantalPollPogingen, boolean technischeFout, int opeenvolgendeRunsZonderVoortgang, int opeenvolgendeTechnischeFouten)
	{
		var sendBevestigingProperties = communicatieHubClientConfig.getBriefafdrukopdrachtSendBevestiging();
		var factor = Math.clamp(aantalPollPogingen, 1, 12);
		var max = sendBevestigingProperties.getMaxPollIntervalMs();
		var interval = sendBevestigingProperties.getInitPollIntervalMs();
		for (var i = 1; i < factor && interval < max; i++)
		{
			interval = verdubbelMetBovengrens(interval, max);
		}
		var outageThreshold = sendBevestigingProperties.getOutageThreshold();
		var outageModeActief = technischeFout && opeenvolgendeTechnischeFouten >= outageThreshold;
		if (outageModeActief || opeenvolgendeRunsZonderVoortgang >= outageThreshold)
		{
			var outageMultiplier = sendBevestigingProperties.getOutageMultiplier();
			interval = multipliceerMetBovengrens(interval, max, outageMultiplier);
		}
		return interval;
	}

	private static @NonNull Long verdubbelMetBovengrens(long interval, long max)
	{
		return multipliceerMetBovengrens(interval, max, 2);
	}

	private static long multipliceerMetBovengrens(long interval, long max, int outageMultiplier)
	{
		return interval > max / outageMultiplier ? max : interval * outageMultiplier;
	}

	private void verwerkSendBevestigingRunResultaat(SendBevestigingRunStatus runStatus)
	{
		if (runStatus.voortgangGeboekt)
		{
			queueStateService.resetSendBevestigingVoortgangState();
			logCommunicatieProblemenOpgelost("wachten op de CommHub send bevestiging", wachtOpSendBevestigingProblemen);
		}
		else if (!runStatus.hadPendingBerichten)
		{
			queueStateService.resetSendBevestigingVoortgangState();
		}
		else
		{
			queueStateService.verhoogOpeenvolgendeSendBevestigingRunsZonderVoortgang();
			if (runStatus.technischeFoutOpgetreden)
			{
				queueStateService.verhoogOpeenvolgendeSendBevestigingTechnischeFouten();
			}
			else
			{
				queueStateService.resetOpeenvolgendeSendBevestigingTechnischeFouten();
			}
		}
	}
}
