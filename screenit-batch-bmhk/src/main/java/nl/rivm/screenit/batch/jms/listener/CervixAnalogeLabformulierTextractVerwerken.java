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

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.util.Optional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixVerwerkTextractLabformulierService;
import nl.rivm.screenit.batch.service.TextractService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.aws.AwsSqsService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.specification.cervix.CervixBMHKLaboratoriumSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.Message;
import software.amazon.awssdk.services.textract.model.AnalyzeDocumentResponse;

@Slf4j
@Configuration
@EnableScheduling
@RequiredArgsConstructor
@ConditionalOnProperty(value = "textract.enabled", havingValue = "true")
public class CervixAnalogeLabformulierTextractVerwerken
{
	private final TextractService textractService;

	private final CervixVerwerkTextractLabformulierService verwerkTextractLabformulierService;

	private final AwsSqsService sqsService;

	@Qualifier("sqsClientTextract")
	private final SqsClient sqsClient;

	@Qualifier("queueNameTextract")
	private final String sqsQueueName;

	@Qualifier("textractStubMode")
	private final boolean stubMode;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BmhkLaboratoriumRepository laboratoriumRepository;

	private final CervixLabformulierService labformulierService;

	private final LogService logService;

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Scheduled(fixedDelay = 10000)
	public void verwerkLabformulieren()
	{
		LOG.info("Ophalen van labformulieren uit Queue");
		Optional<Message> bericht;
		do
		{
			bericht = sqsService.haalEenBerichtUitSqsOp(sqsClient, sqsQueueName);
			LOG.info(bericht.isEmpty() ? "Geen bericht gevonden" : "Bericht gevonden");
			if (bericht.isPresent())
			{
				if (!stubMode)
				{
					analyseerSqsBerichtEnVerwerkLabformulieren(bericht.get());
				}
				else
				{

					LOG.info("Zit in stubmode, Bericht wordt niet naar textract gestuurd.");

				}
				sqsService.verwijderBerichtVanSqsQueue(sqsClient, bericht.get().receiptHandle(), sqsQueueName);
			}
		}
		while (bericht.isPresent());
	}

	private void analyseerSqsBerichtEnVerwerkLabformulieren(Message bericht)
	{
		try
		{
			LOG.info("Verwerken van labformulier gestart");
			var sqsBerichtJson = objectMapper.readTree(bericht.body()).get("Records");

			sqsBerichtJson.forEach(b ->
			{
				if (b.get("eventName").asText().equals("ObjectCreated:Put"))
				{
					var bucketName = b.get("s3").get("bucket").get("name").asText();
					var bestandsnaam = b.get("s3").get("object").get("key").asText();

					var geanalyseerdDocument = textractService.analyseerFormDocument(bucketName, bestandsnaam);
					geanalyseerdDocument.ifPresent(documentResponse -> koppelTextractAanLabformulierEnSlaOp(documentResponse, bestandsnaam));
				}
			});
		}
		catch (JsonProcessingException e)
		{
			LOG.error("Kon geen JSON van bericht maken {}", e.getMessage());
		}
	}

	private void koppelTextractAanLabformulierEnSlaOp(AnalyzeDocumentResponse documentResponse, String bestandsnaam)
	{
		try
		{
			var labformulier = verwerkTextractLabformulierService.vewerkTextractLabformulierResponse(documentResponse);
			labformulier.setObjid(bestandsnaam);
			labformulier.setScanDatum(haalDatumUitBestandsnaam(bestandsnaam));
			labformulier.setLaboratorium(haalLaboratoriumOpUitBestandsnaam(bestandsnaam));
			labformulier.setBarcode(haalWaardeOpUitBestandsnaam(bestandsnaam, "uid"));
			labformulier.setStatus(CervixLabformulierStatus.GESCAND);
			labformulier.setStatusDatum(currentDateSupplier.getDate());
			labformulierService.koppelEnBewaarLabformulier(labformulier);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_GESCAND, new LogEvent("Laboratorium: " + labformulier.getLaboratorium().getNaam()
				+ ", Monster-id: " + labformulier.getBarcode()
				+ ", ObjectID: " + labformulier.getObjid()), Bevolkingsonderzoek.CERVIX);
		}
		catch (ArrayIndexOutOfBoundsException | DateTimeParseException e)
		{
			LOG.error("Bestandsnaam {} heeft geen geldige datum tijd", bestandsnaam, e);
		}
		catch (IllegalArgumentException e)
		{
			LOG.error("Fout bij verwerken van data uit labformulier met bestandsnaam: {}", bestandsnaam, e);
		}
	}

	private Date haalDatumUitBestandsnaam(String bestandsnaam)
	{

		var bestandsPathArray = bestandsnaam.split("/");
		var alleenBestandsnaam = bestandsPathArray[bestandsPathArray.length - 1];
		var opgesplitsteStringBlocks = alleenBestandsnaam.split("_");
		var formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss");
		var datumTijdString = String.format("%s_%s", opgesplitsteStringBlocks[0], opgesplitsteStringBlocks[1]);
		return DateUtil.toUtilDate(LocalDateTime.parse(datumTijdString, formatter));
	}

	private BMHKLaboratorium haalLaboratoriumOpUitBestandsnaam(String bestandsnaam)
	{
		var userIdScanner = haalWaardeOpUitBestandsnaam(bestandsnaam, "scannerid");
		return laboratoriumRepository.findOne(CervixBMHKLaboratoriumSpecification.heeftUserIdScanner(userIdScanner)).orElse(null);
	}

	private String haalWaardeOpUitBestandsnaam(String bestandsnaam, String zoekWoordUitBestandsnaam)
	{
		if (!StringUtils.contains(bestandsnaam, zoekWoordUitBestandsnaam))
		{
			throw new IllegalArgumentException("Bestandsnaam bevat geen zoekWoordUitBestandsnaam: " + zoekWoordUitBestandsnaam);
		}
		var waardeAchterZoekwoordRuw = bestandsnaam.split(zoekWoordUitBestandsnaam)[1];
		return (waardeAchterZoekwoordRuw.split("_"))[0].split("\\.")[0]; 
	}
}
