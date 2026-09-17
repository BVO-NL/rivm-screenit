package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHL7v24HpvOrderTriggerDto;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.cervix.CervixFoutHL7v2BerichtRepository;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.cervix.CervixBaseDossierService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.util.ProjectUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;

@Service
@AllArgsConstructor
@Slf4j
@Transactional
public class CervixBaseDossierServiceImpl implements CervixBaseDossierService
{
	private static final int HPV_ORDER_QUEUE_FETCH_SIZE = 500;

	private final HibernateService hibernateService;

	private final CervixBaseScreeningrondeService baseScreeningrondeService;

	private final CervixFoutHL7v2BerichtRepository foutHL7v2BerichtRepository;

	private final BaseClientContactService clientContactService;

	private final BaseDossierService baseDossierService;

	private final ClientService clientService;

	private final MessageService messageService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional
	public void maakDossierLeeg(Long clientId)
	{
		var dossier = clientService.getClientById(clientId).map(Client::getCervixDossier);
		dossier.ifPresent(d -> maakDossierLeeg(d, true));
	}

	@Override
	public void maakDossierLeeg(CervixDossier dossier, boolean alleAfmeldingen)
	{
		if (dossier == null)
		{
			return;
		}

		try
		{
			var client = dossier.getClient();

			verwijderFoutHl7V2Berichten(client);
			verwijderHpvOrderQueueBerichtenVoorMonstersInDossier(dossier);

			baseScreeningrondeService.verwijderScreeningRondes(dossier);

			clientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.CERVIX);

			if (alleAfmeldingen)
			{
				baseDossierService.verwijderAlleAfmeldingenUitDossier(dossier);
			}
			else
			{
				baseDossierService.verwijderNietLaatsteDefinitieveAfmeldingenUitDossier(dossier);
			}
			verwijderCisHistorie(dossier.getCisHistorie());
			opruimenDossier(dossier);

			hibernateService.saveOrUpdate(client);

			var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
			if (projectClient != null)
			{
				clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.VERWIJDERING_VAN_DOSSIER, Bevolkingsonderzoek.CERVIX);
			}

			LOG.info("Dossier van client '{}' geleegd", dossier.getClient().getId());
		}
		catch (Exception ex)
		{
			LOG.error("Fout bij legen van dossier van client '{}'", dossier.getClient().getId(), ex);
		}
	}

	private void verwijderFoutHl7V2Berichten(Client client)
	{
		var foutBerichten = foutHL7v2BerichtRepository.findAllByClient(client);
		foutHL7v2BerichtRepository.deleteAll(foutBerichten);
	}

	private void verwijderHpvOrderQueueBerichtenVoorMonstersInDossier(CervixDossier dossier)
	{
		var monsterIdsPerLaboratorium = new HashMap<Long, Set<Long>>();
		for (var ronde : dossier.getScreeningRondes())
		{
			for (var uitnodiging : ronde.getUitnodigingen())
			{
				var monster = uitnodiging.getMonster();
				if (monster == null || monster.getId() == null)
				{
					continue;
				}
				var laboratorium = monster.getLaboratorium();
				if (laboratorium == null || laboratorium.getId() == null)
				{
					continue;
				}
				monsterIdsPerLaboratorium.putIfAbsent(laboratorium.getId(), new HashSet<>());
				monsterIdsPerLaboratorium.get(laboratorium.getId()).add(monster.getId());
			}
		}

		for (var monsterIdsPerLab : monsterIdsPerLaboratorium.entrySet())
		{
			verwijderHpvOrderQueueBerichten(monsterIdsPerLab.getKey(), monsterIdsPerLab.getValue());
		}
	}

	private void verwijderHpvOrderQueueBerichten(Long laboratoriumId, Set<Long> monsterIds)
	{
		var context = laboratoriumId.toString();
		var berichten = messageService.fetchMessages(MessageType.HPV_ORDER, context, HPV_ORDER_QUEUE_FETCH_SIZE);
		while (!berichten.isEmpty())
		{
			for (var bericht : berichten)
			{
				verwijderHpvOrderQueueBerichtVoorMonster(bericht, monsterIds);
			}
			var laatsteMessageId = berichten.getLast().getId();
			berichten = messageService.fetchMessagesGroterDanId(MessageType.HPV_ORDER, context, laatsteMessageId, HPV_ORDER_QUEUE_FETCH_SIZE);
		}
	}

	private void verwijderHpvOrderQueueBerichtVoorMonster(Message bericht, Set<Long> monsterIds)
	{
		CervixHL7v24HpvOrderTriggerDto triggerDto;
		try
		{
			triggerDto = messageService.getContent(bericht);
		}
		catch (JsonProcessingException e)
		{
			throw new RuntimeException(e);
		}
		if (triggerDto != null && monsterIds.contains(triggerDto.getMonsterId()))
		{
			messageService.dequeueMessage(bericht);
		}
	}

	private void opruimenDossier(CervixDossier dossier)
	{
		dossier.setInactiefVanaf(null);
		dossier.setInactiefTotMet(null);
		var vooraankondigingsBrief = dossier.getVooraankondigingsBrief();
		if (vooraankondigingsBrief != null)
		{
			dossier.setVooraankondigingsBrief(null);
			hibernateService.delete(vooraankondigingsBrief);
		}

		if (DossierStatus.INACTIEF.equals(dossier.getStatus()) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
		}
		hibernateService.saveOrUpdate(dossier);
	}

	private void verwijderCisHistorie(CervixCISHistorie cisHistorie)
	{
		if (cisHistorie != null)
		{
			hibernateService.deleteAll(cisHistorie.getCisHistorieRegels());
			cisHistorie.getDossier().setCisHistorie(null);
			hibernateService.delete(cisHistorie);
		}
	}
}
