package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.ApplicationEnvironment;
import nl.rivm.screenit.main.service.ClientenVerwijderenTestService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.exception.VerwijderClientException;
import nl.rivm.screenit.model.logging.colon.ColonNieuwFitAanvraagLogEvent;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixTestService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseTestService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ImmutableMap;

@Slf4j
@Service
@Transactional
public class ClientenVerwijderenTestServiceImpl implements ClientenVerwijderenTestService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonTestService colonTestService;

	@Autowired
	private CervixTestService cervixTestService;

	@Autowired
	private MammaBaseTestService mammaBaseTestService;

	@Autowired
	private TestService testService;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private String applicationEnvironment;

	@Autowired
	private BezwaarService bezwaarService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	public String clientenVerwijderen(String bsns)
	{
		var verwijderdeClienten = 0;
		var bsnList = bsns.split(",");
		var result = "Succesvol";
		for (var bsn : bsnList)
		{
			Long clientId = null;
			try
			{
				if (StringUtils.isBlank(bsn) || bsn.trim().length() != 9)
				{
					continue;
				}
				bsn = bsn.trim();
				var client = clientService.getClientByBsn(bsn);
				if (client == null)
				{
					continue;
				}

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

				var logEvents = hibernateService.getByParameters(ColonNieuwFitAanvraagLogEvent.class, ImmutableMap.of("client", client));
				logEvents.forEach(le -> hibernateService.delete(le.getLogRegel()));

				bezwaarService.verwijderBezwaarMomenten(client);
				var gbaVragen = client.getGbaVragen();
				hibernateService.deleteAll(gbaVragen);

				var clientContacten = client.getContacten();
				hibernateService.deleteAll(clientContacten);

				clientId = client.getId();
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

				verwijderdeClienten++;

				LOG.info("Client met BSN '{}' verwijderd en client id '{}'", bsn, clientId);
			}
			catch (VerwijderClientException e)
			{
				result = e.getMessage();
				LOG.error("error bij bsn '{}' met client id '{}'", bsn, clientId, e);
			}
			catch (Exception e)
			{
				result = "Fout bij verwijderen van client met BSN " + bsn;
				LOG.error("error bij bsn '{}' met client id '{}'", bsn, clientId, e);
			}
		}
		return result + ". #" + verwijderdeClienten + " clienten verwijderd.";
	}

	private boolean heeftBeelden(Client client)
	{
		var dossier = client.getMammaDossier();
		if (dossier == null)
		{
			return false;
		}

		return dossier.getScreeningRondes().stream().anyMatch(ronde -> screeningrondeService.heeftBeelden(ronde));
	}
}
