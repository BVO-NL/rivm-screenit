package nl.rivm.screenit.mamma.se.proxy;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.event.ContextRefreshedEvent;

@SpringBootApplication
@ComponentScan(basePackages = { "nl.rivm.screenit" })
@Slf4j
public class SeProxyApplication implements ApplicationListener<ContextRefreshedEvent>
{
	@Autowired
	private SeRestSocketService seRestSocketService;

	@Autowired
	private SeStatusService seStatusService;

	@Autowired
	private ProxyService proxyService;

	@Autowired
	private AchtergrondRequestService achtergrondRequestService;

	@Autowired
	private MammaScreeningsEenheidStatusService statusService;

	private boolean initPerformed = false;

	public static void main(String[] args)
	{
		new SpringApplicationBuilder()
			.sources(SeProxyApplication.class)
			.run(args);
	}

	@Bean(name = "databasePath")
	public String databasePath(@Value("${spring.datasource.url}") String path)
	{
		return path;
	}

	@Override
	public void onApplicationEvent(@NotNull ContextRefreshedEvent contextRefreshedEvent)
	{
		if (!initPerformed)
		{
			initPerformed = true;
			LOG.info("Spring context ready: init SE-code");
			seStatusService.initSeCode();
			LOG.info("Spring context ready: init proxyservice");
			proxyService.init();
			LOG.info("Spring context ready: start achtergrondophaler");
			achtergrondRequestService.ensureRunning();
			LOG.info("Spring context ready: start websocketverbinding met centraal");
			seRestSocketService.initRestSocketService();
			LOG.info("Spring context ready: stuur status naar centraal");
			statusService.maakStatusEnQueueRequestNaarCentraal();
		}
		else
		{
			LOG.info("OnApplicationEvent called when init already performed. Do nothing");
		}
	}
}
