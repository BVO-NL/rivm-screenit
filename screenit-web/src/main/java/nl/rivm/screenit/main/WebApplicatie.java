package nl.rivm.screenit.main;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.factory.colon.DefaultVrijSlotFactory;
import nl.rivm.screenit.repository.impl.BaseJpaRepositoryImpl;
import nl.topicuszorg.wicket.session.DozerSessionFinderImpl;
import nl.topicuszorg.zorgid.webservice.ZorgidWebservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.scheduling.annotation.EnableAsync;

@Slf4j
@EnableJpaRepositories(basePackages = { "nl.rivm.screenit", "nl.topicuszorg.wicket.password.repository" }, repositoryBaseClass = BaseJpaRepositoryImpl.class)
@ComponentScan(
	basePackages = { "nl.rivm.screenit", "nl.topicuszorg" },
	excludeFilters = {
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = ZorgidWebservice.class),
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = DefaultVrijSlotFactory.class),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.wicket.session.DozerSessionFinderImpl"),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.hibernate.spring.module.test.impl.TestServiceImpl"),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.loginformatie.services.impl.LogInformatieServiceImpl"),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.hl7.*")
	})
@EnableAsync
@SpringBootApplication
public class WebApplicatie
{

	public static void main(String[] args)
	{
		SpringApplication.run(WebApplicatie.class, args);
	}

}
