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

import lombok.Setter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConfigurationProperties(prefix = "hl7")
@Setter
public class HL7Config
{

	private String hpvHost;

	private Integer hpvPort;

	private String ifobtHost;

	private Integer ifobtPort;

	private Integer imsPort;

	private Integer ilmPort;

	private String versionMapping;

	@Bean
	String hpvHost()
	{
		return StringUtils.defaultIfBlank(hpvHost, "");
	}

	@Bean
	Integer hpvPort()
	{
		return hpvPort != null ? hpvPort : 0;
	}

	@Bean
	String hl7IfobtHost()
	{
		return StringUtils.defaultIfBlank(ifobtHost, "");
	}

	@Bean
	Integer hl7IfobtPort()
	{
		return ifobtPort != null ? ifobtPort : 0;
	}

	@Bean
	Integer hl7ImsPort()
	{
		return imsPort != null ? imsPort : 0;
	}

	@Bean
	Integer hl7IlmPort()
	{
		return ilmPort != null ? ilmPort : 0;
	}

	@Bean
	String versionMapping()
	{
		return StringUtils.defaultIfBlank(versionMapping, "");
	}

}
