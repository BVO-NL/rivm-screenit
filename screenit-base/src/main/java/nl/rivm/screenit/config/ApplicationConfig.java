package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.nio.file.Path;

import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConfigurationProperties(prefix = "app")
@Setter
public class ApplicationConfig
{
	private Boolean testModus;

	private String environment;

	private String instance;

	private String name;

	private String url;

	private String filestoreLocation;

	private String planningBkRestUrl;

	private String kansberekeningServiceUrl;

	private String asposeLicense;

	private String asposeVragenlijstTemplate;

	private Integer planAhead;

	private Integer maxRoosterUitrolInMonths;

	private String zooKeeperServerUri;

	private String medewerkerPortaalResourceUrl;

	private String activemqUsername;

	private String activemqPassword;

	@Bean
	Boolean isJpa()
	{
		return true;
	}

	@Bean
	String applicationEnvironment()
	{
		return environment;
	}

	@Bean
	String applicationName()
	{
		return name;
	}

	@Bean
	String applicationInstance()
	{
		return instance;
	}

	@Bean
	String applicationUrl()
	{
		return StringUtils.defaultIfBlank(url, "");
	}

	@Bean
	String planningBkRestUrl()
	{
		return StringUtils.defaultIfBlank(planningBkRestUrl, "");
	}

	@Bean
	String kansberekeningServiceUrl()
	{
		return StringUtils.defaultIfBlank(kansberekeningServiceUrl, "");
	}

	@Bean
	@Profile("!test")
	String locatieFilestore()
	{
		return Path.of(filestoreLocation).toString();
	}

	@Bean
	Boolean testModus()
	{
		return testModus;
	}

	@Bean
	String asposeLicence()
	{
		return StringUtils.defaultIfBlank(asposeLicense, filestoreLocation + "/aspose/Aspose.Words.lic");
	}

	@Bean
	String vragenlijstTemplate()
	{
		return StringUtils.defaultIfBlank(asposeVragenlijstTemplate, "");
	}

	@Bean
	Integer planAhead()
	{
		return planAhead != null ? planAhead : 0;
	}

	@Bean
	Integer maxRoosterUitrolInMonths()
	{
		return maxRoosterUitrolInMonths != null ? maxRoosterUitrolInMonths : 15;
	}

	@Bean
	String zooKeeperServerUri()
	{
		return StringUtils.defaultIfBlank(zooKeeperServerUri, "");
	}

	@Bean
	String medewerkerPortaalResourceUrl()
	{
		return StringUtils.defaultIfBlank(medewerkerPortaalResourceUrl, "");
	}

	@Bean
	String activemqUsername()
	{
		return StringUtils.defaultIfBlank(activemqUsername, "");
	}

	@Bean
	String activemqPassword()
	{
		return StringUtils.defaultIfBlank(activemqPassword, "");
	}

}
