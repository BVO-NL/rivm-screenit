package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.config.SqsConfig;
import nl.rivm.screenit.util.aws.SqsUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import software.amazon.awssdk.services.sqs.SqsClient;

@Configuration
@ConfigurationProperties(prefix = "textract")
@Setter
public class TextractConfig
{
	private String endpointOverride;

	private String accessId;

	private String accessSecret;

	private SqsConfig sqs;

	private boolean stubMode;

	@Bean
	String textractEndpointOverride()
	{
		return StringUtils.defaultIfBlank(endpointOverride, "");
	}

	@Bean
	String textractAccessId()
	{
		return StringUtils.defaultIfBlank(accessId, "");
	}

	@Bean
	String textractAccessSecret()
	{
		return StringUtils.defaultIfBlank(accessSecret, "");
	}

	@Bean
	public boolean textractStubMode()
	{
		return stubMode;
	}

	@Bean
	@Scope("prototype")
	public SqsClient sqsClientTextract()
	{
		return SqsUtil.sqsClient(sqs);
	}

	@Bean
	public String queueNameTextract()
	{
		return SqsUtil.queueName(sqs);
	}
}
