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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.CervixAnalogeLabformulierS3VerwerkenService;
import nl.rivm.screenit.batch.service.CervixVerwerkTextractLabformulierService;
import nl.rivm.screenit.batch.service.TextractService;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.repository.cervix.CervixLabformulierRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.specification.cervix.CervixBMHKLaboratoriumSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
@ConditionalOnProperty(value = "textract.enabled", havingValue = "true")
public class CervixAnalogeLabformulierS3VerwerkenServiceImpl implements CervixAnalogeLabformulierS3VerwerkenService
{
	private final TextractService textractService;

	private final CervixVerwerkTextractLabformulierService verwerkTextractLabformulierService;

	@Qualifier("textractStubMode")
	private final boolean stubMode;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BmhkLaboratoriumRepository laboratoriumRepository;

	private final CervixLabformulierService labformulierService;

	private final LogService logService;

	private final CervixLabformulierRepository labformulierRepository;

	@Transactional
	public void verwerkLabformulier(String bestandsnaam, String bucketName)
	{
		var labformulier = verwerkLabformulierMetTextract(bestandsnaam, bucketName);
		haalWaardenUitBestandsnaamEnSlaOp(labformulier, bestandsnaam);
	}

	private CervixLabformulier verwerkLabformulierMetTextract(String bestandsnaam, String bucketName)
	{
		if (!stubMode)
		{
			try
			{
				var geanalyseerdDocument = textractService.analyseerFormDocument(bestandsnaam, bucketName);
				return verwerkTextractLabformulierService.vewerkTextractLabformulierResponse(geanalyseerdDocument);
			}
			catch (Exception e)
			{
				throw new IllegalArgumentException("Fout bij verwerken labformulier met naam: %s".formatted(bestandsnaam), e);
			}
		}
		else
		{
			var labformulier = new CervixLabformulier();
			labformulier.setDigitaal(false);
			return labformulier;
		}
	}

	private void haalWaardenUitBestandsnaamEnSlaOp(CervixLabformulier labformulier, String bestandsnaam)
	{
		labformulier.setObjid(bestandsnaam);
		labformulier.setScanDatum(haalDatumUitBestandsnaam(bestandsnaam));
		labformulier.setLaboratorium(haalLaboratoriumOpUitBestandsnaam(bestandsnaam));
		labformulier.setBarcode(haalWaardeOpUitBestandsnaam(bestandsnaam, "uid"));
		labformulier.setStatus(CervixLabformulierStatus.GESCAND);
		labformulier.setStatusDatum(currentDateSupplier.getDate());
		labformulierRepository.save(labformulier);
		labformulierService.koppelEnBewaarLabformulier(labformulier);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_GESCAND,
			new LogEvent("Laboratorium: " + labformulier.getLaboratorium().getNaam() + ", Monster-id: " + labformulier.getBarcode() + ", ObjectID: " + labformulier.getObjid()),
			Bevolkingsonderzoek.CERVIX);
		LOG.info("Labformulier met barcode {} en objectId {} is succesvol verwerkt.", labformulier.getBarcode(), labformulier.getObjid());
	}

	private Date haalDatumUitBestandsnaam(String bestandsnaam)
	{
		try
		{

			var bestandsPathArray = bestandsnaam.split("/");
			var alleenBestandsnaam = bestandsPathArray[bestandsPathArray.length - 1];
			var opgesplitsteStringBlocks = alleenBestandsnaam.split("_");
			var formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy_HH-mm-ss");
			var datumTijdString = String.format("%s_%s", opgesplitsteStringBlocks[0], opgesplitsteStringBlocks[1]);
			return DateUtil.toUtilDate(LocalDateTime.parse(datumTijdString, formatter));
		}
		catch (DateTimeParseException e)
		{
			throw new IllegalArgumentException("Fout bij parsen scandatum uit bestandsnaam: %s".formatted(bestandsnaam), e);
		}
	}

	private BMHKLaboratorium haalLaboratoriumOpUitBestandsnaam(String bestandsnaam)
	{
		var userIdScanner = haalWaardeOpUitBestandsnaam(bestandsnaam, "scannerid");
		return laboratoriumRepository.findOne(CervixBMHKLaboratoriumSpecification.heeftUserIdScanner(userIdScanner))
			.orElseThrow(() -> new IllegalArgumentException("Geen lab gevonden bij scannerId uit bestandsnaam: " + bestandsnaam));
	}

	private String haalWaardeOpUitBestandsnaam(String bestandsnaam, String zoekWoordUitBestandsnaam)
	{
		if (!StringUtils.contains(bestandsnaam, zoekWoordUitBestandsnaam))
		{
			throw new IllegalArgumentException("Bestandsnaam %s bevat geen zoekWoordUitBestandsnaam: %s".formatted(bestandsnaam, zoekWoordUitBestandsnaam));
		}
		var waardeAchterZoekwoordRuw = bestandsnaam.split(zoekWoordUitBestandsnaam)[1];
		return (waardeAchterZoekwoordRuw.split("[_.]"))[0]; 
	}
}
