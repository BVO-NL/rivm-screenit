package nl.rivm.screenit.batch.service.impl;

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

import java.net.URI;

import jakarta.annotation.PostConstruct;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.TextractService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.textract.TextractClient;
import software.amazon.awssdk.services.textract.TextractClientBuilder;
import software.amazon.awssdk.services.textract.model.AnalyzeDocumentRequest;
import software.amazon.awssdk.services.textract.model.AnalyzeDocumentResponse;
import software.amazon.awssdk.services.textract.model.FeatureType;

@Slf4j
@Service
@RequiredArgsConstructor
public class TextractServiceImpl implements TextractService
{
	private final String textractAccessId;

	private final String textractAccessSecret;

	@Qualifier("textractEndpointOverride")
	private final String endpointOverride;

	private TextractClient textractClient;

	@PostConstruct
	private void postConstruct()
	{
		var basicCredentials = AwsBasicCredentials.create(textractAccessId, textractAccessSecret);
		textractClient = TextractClient.builder().credentialsProvider(StaticCredentialsProvider.create(basicCredentials)).region(Region.EU_WEST_1)
			.applyMutation(this::setEndpointOverrideIfPresent).build();
	}

	@Override
	public AnalyzeDocumentResponse analyseerFormDocument(String bestandsnaam, String bucketName)
	{
		var analyzeDocumentRequest = AnalyzeDocumentRequest.builder().featureTypes(FeatureType.FORMS)
			.document(document -> document.s3Object(s3Object -> s3Object.bucket(bucketName).name(bestandsnaam))).build();
		return textractClient.analyzeDocument(analyzeDocumentRequest);
	}

	private void setEndpointOverrideIfPresent(TextractClientBuilder textractClientBuilder)
	{
		if (StringUtils.isNotBlank(endpointOverride))
		{
			var endpointOverrideURI = URI.create(endpointOverride);
			textractClientBuilder.endpointOverride(endpointOverrideURI);
		}
	}

}
