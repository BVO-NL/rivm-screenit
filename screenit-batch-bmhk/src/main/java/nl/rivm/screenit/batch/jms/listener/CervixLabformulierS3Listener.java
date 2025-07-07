package nl.rivm.screenit.batch.jms.listener;

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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixAnalogeLabformulierS3VerwerkenService;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.awspring.cloud.sqs.annotation.SqsListener;

@Slf4j
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(value = "textract.enabled", havingValue = "true")
public class CervixLabformulierS3Listener
{
	private static final ObjectMapper objectMapper = new ObjectMapper();

	private final CervixAnalogeLabformulierS3VerwerkenService analogeLabformulierenS3VerwerkenService;

	private final LogService logService;

	@SqsListener(queueNames = "#{queueNameTextract}", factory = "textractSqsListenerContainerFactory")
	protected void verwerkLabformulierSqsBericht(String sqsBericht)
	{
		try
		{
			var sqsBerichtJson = objectMapper.readTree(sqsBericht).get("Records");
			sqsBerichtJson.forEach(b ->
			{
				if (b.get("eventName").asText().equals("ObjectCreated:Put"))
				{
					var bucketName = b.get("s3").get("bucket").get("name").asText();
					var bestandsnaam = b.get("s3").get("object").get("key").asText();
					analogeLabformulierenS3VerwerkenService.verwerkLabformulier(bestandsnaam, bucketName);
				}
			});
		}
		catch (Exception e)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_S3_LABFORMULIER_FOUT_ONTVANGEN, "Fout bij ontvangst gescand labformulier: %s".formatted(e.getMessage()),
				Bevolkingsonderzoek.CERVIX);
			LOG.error("Fout bij verwerken van labformulier SQS bericht", e);
		}
	}
}
