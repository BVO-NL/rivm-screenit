package nl.rivm.screenit.mamma.se.proxy.cron;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
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

import nl.rivm.screenit.mamma.se.proxy.services.AuthenticatieService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Configuration
@EnableScheduling
public class OfflineWerkenCleaner
{
	private static final Logger LOG = LoggerFactory.getLogger(OfflineWerkenCleaner.class);

	private static final String JOB_OMSCHRIJVING = "Opruimen van ingelogde gebrukers";

	@Autowired
	private AuthenticatieService authenticatieService;

	@Scheduled(cron = "0 0 * * * ?")
	public void verwijderOudeIngelogdeGebruikers()
	{
		LOG.debug("Start: " + JOB_OMSCHRIJVING);
		authenticatieService.verwijderOudeIngelogdeGebruikers();
		LOG.debug("Stop: " + JOB_OMSCHRIJVING);
	}
}
