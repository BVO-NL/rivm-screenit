package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import jakarta.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixHerindexeerVerrichtingenService;
import nl.rivm.screenit.batch.service.CervixVerwijderSepaDataService;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHerindexatieDto;
import nl.rivm.screenit.model.messagequeue.dto.VerwijderBetaalOpdrachtDto;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.MessageService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;

@Slf4j
@Service
public class CervixVerrichtingenQueueServiceImpl
{
	@Autowired
	private CervixHerindexeerVerrichtingenService herindexatieService;

	@Autowired
	private CervixVerwijderSepaDataService verwijderSepaDataService;

	@Autowired
	private MessageService messageService;

	@Autowired
	private DatabaseRunner databaseRunner;

	private int heartbeatCounter = 0;

	@PostConstruct
	public void init()
	{

		LOG.info(
			"Queue gestart voor het verwerken van verrichtingen met de params: BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE={} en BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS= {}",
			Integer.getInteger("BMHK_VERRICHTINGEN_HERINDEXEREN_CHUNK_SIZE", 500),
			Integer.getInteger("BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS", 200));

		var executorService = Executors.newSingleThreadExecutor();
		executorService.submit(() ->
		{

			while (true)
			{
				herindexeer();

				verwijderBetaalOpdracht();
			}
		});

	}

	private void herindexeer()
	{
		databaseRunner.runInSessionOnly(() ->
		{
			var totaalAantalVerrichtingen = new AtomicInteger(0);
			CervixHerindexatieDto herindexatieDto = null;
			try
			{
				var message = messageService.getOldestMessage(MessageType.HERINDEXATIE);
				if (message.isPresent())
				{
					herindexatieDto = messageService.getContent(message.get());
					totaalAantalVerrichtingen.set(0);
					verwerkHerindexeringMessage(herindexatieDto, totaalAantalVerrichtingen);
					messageService.dequeueMessage(message.get());
				}
			}
			catch (InterruptedException | JsonProcessingException e)
			{
				herindexatieService.logFout(herindexatieDto, totaalAantalVerrichtingen.get(), e);
				Thread.currentThread().interrupt();
			}
		});
	}

	private void verwijderBetaalOpdracht()
	{
		databaseRunner.runInSessionOnly(() ->
		{
			VerwijderBetaalOpdrachtDto verwijderBetaalOpdrachtDto = null;
			try
			{
				var message = messageService.getOldestMessage(MessageType.VERWIJDER_BETAAL_OPDRACHT);
				if (message.isPresent())
				{
					verwijderBetaalOpdrachtDto = messageService.getContent(message.get());
					verwerkVerwijderingMessage(verwijderBetaalOpdrachtDto);
					messageService.dequeueMessage(message.get());
				}
			}
			catch (Exception e)
			{
				LOG.error("Fout bij verwerking van verwijder betaal opdracht met id {}", (verwijderBetaalOpdrachtDto != null ? verwijderBetaalOpdrachtDto.getId() : "onbekend"),
					e);
			}
		});
		wachtOpVolgendeOpdracht();
	}

	private void verwerkVerwijderingMessage(VerwijderBetaalOpdrachtDto opdrachtDto)
	{
		var batchSize = 1000;
		var cervixBoekRegels = verwijderSepaDataService.haalBoekRegelsOp(opdrachtDto.getId(), batchSize);
		var teVerwerkenBoekregels = verwijderSepaDataService.aantalTeVerwerkenBoekregels(opdrachtDto.getId());
		while (!cervixBoekRegels.isEmpty())
		{
			LOG.info("{} van de {} regels opgehaald om te ontkoppelen", cervixBoekRegels.size(), teVerwerkenBoekregels);
			verwijderSepaDataService.ontkoppelBoekregelsVanSpecificatie(cervixBoekRegels);
			cervixBoekRegels = verwijderSepaDataService.haalBoekRegelsOp(opdrachtDto.getId(), batchSize);
		}

		var betaalopdracht = verwijderSepaDataService.haalBetaalOpdrachtOp(opdrachtDto.getId());
		verwijderSepaDataService.verwijderBetaalopdracht(betaalopdracht);
	}

	private void verwerkHerindexeringMessage(CervixHerindexatieDto herindexatieDto, AtomicInteger totaalAantalVerrichtingen) throws InterruptedException
	{
		herindexatieService.logStart(herindexatieDto);
		int aantalVerrichtingen;
		do
		{
			aantalVerrichtingen = herindexatieService.verrichtingenHerindexeren(herindexatieDto);
			totaalAantalVerrichtingen.addAndGet(aantalVerrichtingen);
			LOG.info("{} (verdere) verrichtingen zijn aangepast.", aantalVerrichtingen);
			if (herindexatieDto.isHuisartsTarief() && aantalVerrichtingen > 0)
			{

				Thread.sleep(Integer.getInteger("BMHK_HERINDEXEREN_PAUZE_BETWEEN_CHUNKS", 200));
			}
		}
		while (aantalVerrichtingen > 0);

		herindexatieService.logEinde(herindexatieDto, totaalAantalVerrichtingen.get());

	}

	private void wachtOpVolgendeOpdracht()
	{
		try
		{
			Thread.sleep(3000L);
		}
		catch (InterruptedException e)
		{
			LOG.warn("Interrupted!", e);
			Thread.currentThread().interrupt();
		}
		if (heartbeatCounter++ % 20 == 0)
		{
			LOG.info("Heartbeat verwerkVerrichtingVerzoeken");
		}
	}
}
