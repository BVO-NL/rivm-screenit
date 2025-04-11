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

import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Configuration
@ConfigurationProperties(prefix = "s3.labformulieren")
@Setter
public class LabformulierenS3BucketConfig
{
	private String endpointOverride;

	private String accessId;

	private String accessSecret;

	private String region;

	private String name;

	private String omgevingNaam;

	@Bean
	public boolean labformulierenS3bucketEnabled()
	{
		return isNotBlank(accessId) && isNotBlank(accessSecret) && isNotBlank(region) && isNotBlank(name) && isNotBlank(omgevingNaam);
	}

	@Bean
	public String labformulierenS3bucketEndpointOverride()
	{
		return StringUtils.defaultIfBlank(endpointOverride, "");
	}

	@Bean
	public String labformulierenS3bucketAccessId()
	{
		return StringUtils.defaultIfBlank(accessId, "");
	}

	@Bean
	public String labformulierenS3bucketAccessSecret()
	{
		return StringUtils.defaultIfBlank(accessSecret, "");
	}

	@Bean
	public String labformulierenS3bucketRegion()
	{
		return StringUtils.defaultIfBlank(region, "");
	}

	@Bean
	public String labformulierenS3bucketName()
	{
		return StringUtils.defaultIfBlank(name, "");
	}

	@Bean
	public String labformulierenOmgevingNaam()
	{
		return StringUtils.defaultIfBlank(omgevingNaam, "");
	}

}
