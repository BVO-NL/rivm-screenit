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

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import jakarta.persistence.EntityManager;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.CommunicationHubProperties;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
@EnableScheduling
@Profile("!test")
public class BriefafdrukopdrachtQueueMonitorScheduler extends BriefafdrukopdrachtMessageQueueHandler
{
	private static final int QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD = 500;

	private static final int QUEUE_TE_VERSTUREN_SIZE_THRESHOLD = 1000;

	private boolean queueSizeWarning;

	private boolean queueTeVersturenWarning;

	private boolean queueNotYetDequeuedWarning;

	private boolean queueErrorWarning;

	private boolean sendBevestigingVertraagdWarning;

	private long monitorRunCounter;

	public BriefafdrukopdrachtQueueMonitorScheduler(MessageService messageService, BaseBriefService baseBriefService, MessageRepository messageRepository, LogService logService,
		ICurrentDateSupplier currentDateSupplier, DatabaseRunner databaseRunner, BatchApplicationType batchApplicationType, EntityManager entityManager,
		CommunicationHubProperties communicatieHubClientConfig,
		BriefafdrukopdrachtMessageQueueStateService queueStateService, String applicationEnvironment)
	{
		super(messageService, baseBriefService, messageRepository, logService, currentDateSupplier, databaseRunner, batchApplicationType, entityManager,
			communicatieHubClientConfig, queueStateService, applicationEnvironment);
	}

	@Scheduled(fixedDelayString = "${commhub.briefafdrukopdracht-monitor.scheduler-delay-ms:30000}")
	public void monitorBriefafdrukopdrachten()
	{
		databaseRunner.runInSessionOnly(() ->
		{
			var queueSizeTeVersturen = messageService.fetchQueueSize(MessageType.BRIEF_AFDRUKKEN, batchApplicationType.name());
			var queueSizeError = messageService.fetchQueueSize(MessageType.BRIEF_AFDRUKKEN_ERROR, batchApplicationType.name());
			var queueSizeNotYetDequeued = messageService.fetchQueueSize(MessageType.BRIEF_AFDRUKKEN_SEND, batchApplicationType.name());
			var oudsteBerichtenZonderSendBevestiging = messageService.fetchMessages(MessageType.BRIEF_AFDRUKKEN_SEND, batchApplicationType.name(), 1);
			var oudsteBerichtZonderSendBevestiging = oudsteBerichtenZonderSendBevestiging
				.stream()
				.findFirst()
				.orElse(null);

			logQueueSizeProblemen(queueSizeTeVersturen, queueSizeNotYetDequeued, queueSizeError);
			logVertraagdeSendBevestigingAlsNodig(queueSizeNotYetDequeued, oudsteBerichtZonderSendBevestiging);
		});
		monitorRunCounter++;
		if (monitorRunCounter % 6 == 0)
		{
			LOG.info("Heartbeat briefafdrukopdrachten monitor");
		}
	}

	private void logQueueSizeProblemen(Long queueSizeTeVersturen, Long queueSizeNotYetDequeued, Long queueSizeError)
	{
		var oldQueueSizeWarningValue = queueSizeWarning;
		var queueTeVersturenTeGroot = queueSizeTeVersturen > QUEUE_TE_VERSTUREN_SIZE_THRESHOLD;
		var queueNotYetDequeuedTeGroot = queueSizeNotYetDequeued > QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD;
		var queueErrorTeGroot = queueSizeError > 0;
		var nieuweOverschrijding = queueTeVersturenTeGroot && !queueTeVersturenWarning
			|| queueNotYetDequeuedTeGroot && !queueNotYetDequeuedWarning
			|| queueErrorTeGroot && !queueErrorWarning;

		queueSizeWarning = queueTeVersturenTeGroot || queueNotYetDequeuedTeGroot || queueErrorTeGroot;
		queueTeVersturenWarning = queueTeVersturenTeGroot;
		queueNotYetDequeuedWarning = queueNotYetDequeuedTeGroot;
		queueErrorWarning = queueErrorTeGroot;

		if (nieuweOverschrijding)
		{
			LOG.warn("Queue size wordt te groot! Te versturen {}>{}, Nog niet afgemeld {}>{}, Foutmeldingen {}>0",
				queueSizeTeVersturen, QUEUE_TE_VERSTUREN_SIZE_THRESHOLD,
				queueSizeNotYetDequeued, QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD,
				queueSizeError);
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_QUEUE_ERG_GROOT,
				"In een van queues staan te veel berichten: Te versturen %s>%s, Nog niet afgemeld %s>%s, Foutmeldingen %s>0"
					.formatted(queueSizeTeVersturen, QUEUE_TE_VERSTUREN_SIZE_THRESHOLD,
						queueSizeNotYetDequeued, QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD,
						queueSizeError),
				getBevolkingsonderzoeken());
		}
		else if (oldQueueSizeWarningValue && !queueSizeWarning)
		{
			LOG.info("Queue size is weer klein genoeg");
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_QUEUE_NORMAAL,
				"Het aantal berichten in de queue is weer normaal.",
				getBevolkingsonderzoeken());
		}
	}

	private void logVertraagdeSendBevestigingAlsNodig(Long queueSizeNotYetDequeued, Message oudsteBerichtZonderSendBevestiging)
	{
		var duurWachten = oudsteBerichtZonderSendBevestiging == null ? Duration.ZERO
			: Duration.between(DateUtil.toLocalDateTime(oudsteBerichtZonderSendBevestiging.getAanmaakMoment()), currentDateSupplier.getLocalDateTime());
		var waarschuwingDrempelMs = communicatieHubClientConfig.getBriefafdrukopdrachtSendBevestiging().getLangdurigGeenParagonWaarschuwingMs();
		var heeftVertraagdeSendBevestiging = duurWachten.toMillis() >= waarschuwingDrempelMs;
		if (heeftVertraagdeSendBevestiging)
		{
			if (!sendBevestigingVertraagdWarning)
			{
				sendBevestigingVertraagdWarning = true;
				logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_SEND_BEVESTIGING_VERTRAAGD,
					"Een of meer briefafdrukopdrachten hebben niet binnen %s minuten een send bevestiging ontvangen. Oudste bericht wacht al %s minuten (message id: '%s'). Aantal wachtende berichten: %s."
						.formatted(
							TimeUnit.MILLISECONDS.toMinutes(waarschuwingDrempelMs),
							duurWachten.toMinutes(),
							oudsteBerichtZonderSendBevestiging.getId(),
							queueSizeNotYetDequeued),
					getBevolkingsonderzoeken());
			}
		}
		else if (sendBevestigingVertraagdWarning)
		{
			sendBevestigingVertraagdWarning = false;
			logService.logGebeurtenis(LogGebeurtenis.BRIEF_AFDRUK_OPDRACHT_SEND_BEVESTIGING_HERSTELD,
				"Alle briefafdrukopdrachten in de send queue zitten weer binnen de toegestane wachttijd voor send bevestiging.",
				getBevolkingsonderzoeken());
		}
	}
}
