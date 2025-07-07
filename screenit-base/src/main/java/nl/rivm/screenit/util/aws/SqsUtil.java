package nl.rivm.screenit.util.aws;

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

import java.net.URI;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.config.SqsConfig;

import org.apache.commons.lang3.StringUtils;

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sqs.SqsAsyncClient;
import software.amazon.awssdk.services.sqs.SqsBaseClientBuilder;
import software.amazon.awssdk.services.sqs.SqsClient;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class SqsUtil
{
	public static SqsClient sqsClient(SqsConfig sqsConfig)
	{
		var sqsClientBuilder = SqsClient.builder()
			.region(Region.EU_WEST_1)
			.credentialsProvider(StaticCredentialsProvider.create(getAwsBasicCredentials(sqsConfig)));

		overrideEndpointIfNeeded(sqsConfig, sqsClientBuilder);
		return sqsClientBuilder.build();
	}

	private static void overrideEndpointIfNeeded(SqsConfig sqsConfig, SqsBaseClientBuilder<?, ?> sqsClientBuilder)
	{
		if (StringUtils.isNotBlank(sqsConfig.getEndpointOverride()))
		{
			sqsClientBuilder.endpointOverride(URI.create(sqsConfig.getEndpointOverride()));
		}
	}

	public static SqsAsyncClient sqsAsyncClient(SqsConfig sqsConfig)
	{
		var sqsAsyncClientBuilder = SqsAsyncClient.builder()
			.region(Region.EU_WEST_1)
			.credentialsProvider(StaticCredentialsProvider.create(getAwsBasicCredentials(sqsConfig)));

		overrideEndpointIfNeeded(sqsConfig, sqsAsyncClientBuilder);
		return sqsAsyncClientBuilder.build();
	}

	public static String queueName(SqsConfig sqsConfig)
	{
		return sqsConfig == null ? null : sqsConfig.getQueueName();
	}

	private static AwsBasicCredentials getAwsBasicCredentials(SqsConfig sqsConfig)
	{
		if (StringUtils.isNotBlank(sqsConfig.getUsername()) && StringUtils.isNotBlank(sqsConfig.getPassword()))
		{
			return AwsBasicCredentials.create(
				sqsConfig.getUsername(),
				sqsConfig.getPassword());
		}
		throw new IllegalStateException("Geen AWS credentials meegegeven");
	}

}
