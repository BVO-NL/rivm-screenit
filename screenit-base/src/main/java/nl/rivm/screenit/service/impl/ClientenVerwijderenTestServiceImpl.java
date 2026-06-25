package nl.rivm.screenit.service.impl;

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

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.ApplicationEnvironment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.exception.VerwijderClientException;
import nl.rivm.screenit.model.logging.colon.ColonNieuwFitAanvraagLogEvent;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ClientenVerwijderenTestService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;

@Slf4j
@Service
@RequiredArgsConstructor
public class ClientenVerwijderenTestServiceImpl implements ClientenVerwijderenTestService
{
	private final HibernateService hibernateService;

	private final ColonTestService colonTestService;

	private final CervixTestService cervixTestService;

	private final MammaBaseTestService mammaBaseTestService;

	private final TestService testService;

	private final BaseProjectService projectService;

	private final ClientService clientService;

	private final LogService logService;

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final BezwaarService bezwaarService;

	private final UploadDocumentService uploadDocumentService;

	@Qualifier("applicationEnvironment")
	private final String applicationEnvironment;

	@Override
	@Transactional
	public String clientenVerwijderen(String bsns)
	{
		var clienten = new ArrayList<Client>();
		for (var rawBsn : bsns.split(","))
		{
			if (StringUtils.isBlank(rawBsn) || rawBsn.trim().length() != 9)
			{
				continue;
			}
			var bsn = rawBsn.trim();
			var client = clientService.getClientByBsn(bsn);
			if (client != null)
			{
				clienten.add(client);
			}
		}
		return clientenVerwijderen(clienten);
	}

	@Override
	@Transactional
	public String clientenVerwijderen(List<Client> clienten)
	{
		var verwijderdeClienten = 0;
		var result = "Succesvol";
		for (var client : clienten)
		{
			var verwijderResult = verwijderClientVeilig(client);
			if (verwijderResult == null)
			{
				verwijderdeClienten++;
			}
			else
			{
				result = verwijderResult;
			}
		}
		return result + ". #" + verwijderdeClienten + " clienten verwijderd.";
	}

	private String verwijderClientVeilig(Client client)
	{
		Long clientId = client.getId();
		try
		{
			verwijderClient(client);
			LOG.info("Client met id '{}' verwijderd", clientId);
			return null;
		}
		catch (VerwijderClientException e)
		{
			LOG.error("error bij client id '{}'", clientId, e);
			return e.getMessage();
		}
		catch (Exception e)
		{
			LOG.error("error bij client id '{}'", clientId, e);
			return "Fout bij verwijderen van client met id " + clientId;
		}
	}

	private void verwijderClient(Client client) throws VerwijderClientException
	{
		var isKetenEnvironment = ApplicationEnvironment.KTN.getEnvNaam().equalsIgnoreCase(applicationEnvironment);
		if (isKetenEnvironment && heeftBeelden(client))
		{
			throw new VerwijderClientException("Ronde bevat beelden. Client moet eerst gereset worden.");
		}

		projectService.verwijderClientVanProjecten(client);

		cervixTestService.clientReset(client);
		colonTestService.clientReset(client);
		mammaBaseTestService.clientReset(client, true);

		testService.verwijderClientContacten(client, true, true, true);

		logService.verwijderLogRegelsVanClient(client);

		List<ColonNieuwFitAanvraagLogEvent> logEvents = hibernateService.getByParameters(ColonNieuwFitAanvraagLogEvent.class, ImmutableMap.of("client", client));
		logEvents.forEach(le -> hibernateService.delete(le.getLogRegel()));

		bezwaarService.verwijderBezwaarMomenten(client);
		var gbaVragen = client.getGbaVragen();
		hibernateService.deleteAll(gbaVragen);

		var clientContacten = client.getContacten();
		hibernateService.deleteAll(clientContacten);

		var clientId = client.getId();
		hibernateService.executeSql("update screenit_revision_entity set client = null where client = " + clientId);

		var overdrachtPersoonsgegevensLijst = new ArrayList<>(client.getOverdrachtPersoonsgegevensLijst());
		hibernateService.deleteAll(overdrachtPersoonsgegevensLijst);
		client.getOverdrachtPersoonsgegevensLijst().clear();

		var overgeblevenBrieven = hibernateService.getByParameters(AlgemeneBrief.class, ImmutableMap.of("client", client));
		hibernateService.deleteAll(overgeblevenBrieven);

		var overgeblevenOnderzoeksresultatenActies = hibernateService.getByParameters(OnderzoeksresultatenActie.class, ImmutableMap.of("client", client));
		overgeblevenOnderzoeksresultatenActies.forEach(actie -> uploadDocumentService.delete(actie.getGetekendeBrief()));
		hibernateService.deleteAll(overgeblevenOnderzoeksresultatenActies);

		hibernateService.delete(client);
	}

	private boolean heeftBeelden(Client client)
	{
		var dossier = client.getMammaDossier();
		if (dossier == null)
		{
			return false;
		}

		return dossier.getScreeningRondes().stream().anyMatch(screeningrondeService::heeftBeelden);
	}
}
