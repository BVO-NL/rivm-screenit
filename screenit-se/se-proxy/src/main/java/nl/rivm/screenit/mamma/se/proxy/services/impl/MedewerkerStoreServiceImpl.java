package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.proxy.model.TimeCleanableItem;
import nl.rivm.screenit.mamma.se.proxy.services.MedewerkerStoreService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class MedewerkerStoreServiceImpl implements MedewerkerStoreService
{
	private static final Logger LOG = LoggerFactory.getLogger(MedewerkerStoreServiceImpl.class);

	private Map<Long, OrganisatieMedewerkerEntry> seOrganisatieMedewerkers = new ConcurrentHashMap<>();

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public String addOrganisatieMedewerker(long organisatieMedewerkerId, String displayName)
	{
		seOrganisatieMedewerkers.put(organisatieMedewerkerId, new OrganisatieMedewerkerEntry(displayName));
		return maakAddTransactie(organisatieMedewerkerId, displayName);
	}

	@Override
	public Map<Long, String> getSeOrganisatieMedewerkers()
	{
		Map<Long, String> result = new HashMap<>();
		seOrganisatieMedewerkers.forEach((organisatieMedewerkerId, organisatieMedewerkerEntry) -> result.put(organisatieMedewerkerId, organisatieMedewerkerEntry.getDisplayName()));
		return result;
	}

	@Override
	public void clearOldEntries()
	{
		LOG.info("Verwijder opgeslagen medewerkers die vandaag niet hebben ingelogd");
		seOrganisatieMedewerkers.entrySet().removeIf(e -> e.getValue().isOuderDan(DateUtil.getAfgelopenMiddernacht()));
	}

	private String maakAddTransactie(long organisatieMedewerkerId, String displayName)
	{
		ObjectNode transactieNode = objectMapper.createObjectNode();
		ArrayNode actieNodes = transactieNode.putArray("actions");

		ObjectNode actieNode = objectMapper.createObjectNode();
		actieNode.put("type", "ADD_SE_MEDEWERKER");
		actieNode.put("organisatieMedewerkerId", organisatieMedewerkerId);
		actieNode.put("displayName", displayName);
		actieNodes.add(actieNode);
		return transactieNode.toString();
	}

	private static class OrganisatieMedewerkerEntry extends TimeCleanableItem
	{
		private final String displayName;

		private OrganisatieMedewerkerEntry(String displayName)
		{
			this.displayName = displayName;
		}

		public String getDisplayName()
		{
			return displayName;
		}
	}
}
