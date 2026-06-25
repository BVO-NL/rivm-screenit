package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import jakarta.persistence.Column;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MammaCStoreService;
import nl.rivm.screenit.batch.service.impl.dicom.CStoreSCU;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoekStatus;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.model.mamma.dicom.CStoreConfig;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.repository.mamma.MammaUploadBeeldenVerzoekRepository;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.dcm4che3.data.DatePrecision;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.io.DicomInputStream;
import org.dcm4che3.net.Status;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HL7Exception;

@Slf4j
@Service
@RequiredArgsConstructor
public class MammaCStoreServiceImpl implements MammaCStoreService
{
	private final HibernateService hibernateService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	private final UploadDocumentService uploadDocumentService;

	private final MammaBaseFactory baseFactory;

	private final BerichtToBatchService berichtToBatchService;

	private final LogService logService;

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final MammaBaseUitwisselportaalService uitwisselportaalService;

	private final MammaBaseIlmService baseIlmService;

	private final DatabaseRunner databaseRunner;

	private final MammaUploadBeeldenVerzoekRepository uploadBeeldenVerzoekRepository;

	private static int STATUS_MELDING_COLUMN_SIZE;

	static
	{
		try
		{
			STATUS_MELDING_COLUMN_SIZE = MammaUploadBeeldenPoging.class.getDeclaredField("statusMelding").getAnnotation(Column.class).length();
		}
		catch (NoSuchFieldException | SecurityException e)
		{
			LOG.error("Fout bij initialiseren van statusmelding kolom size", e);
		}
	}

	@Override
	public void beeldenOntvangenUploadPoging(MammaUploadBeeldenPoging uploadBeeldenPoging) throws HL7Exception
	{
		if (MammaMammografieIlmStatus.NIET_BESCHIKBAAR.equals(uploadBeeldenPoging.getIlmStatus()))
		{
			uploadBeeldenPoging.setIlmStatus(MammaMammografieIlmStatus.BESCHIKBAAR);
			uploadBeeldenPoging.setIlmStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(uploadBeeldenPoging);
		}
		else
		{
			var melding = String.format("Inkomend CA bericht voor uploadverzoek met accessionnummer %s is al verwerkt op %s en kon niet worden omgezet van %s naar BESCHIKBAAR",
				uploadBeeldenPoging.getAccessionNumber(), uploadBeeldenPoging.getIlmStatusDatum(), uploadBeeldenPoging.getIlmStatus().name());
			throw new HL7Exception(melding);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void beeldenVerwijderdUploadVerzoek(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaIMSBericht bericht, Client client, boolean error)
	{
		uploadBeeldenPoging.setIlmStatus(error ? MammaMammografieIlmStatus.VERWIJDEREN_MISLUKT : MammaMammografieIlmStatus.VERWIJDERD);
		uploadBeeldenPoging.setIlmStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(uploadBeeldenPoging);

		if (error)
		{
			var melding = String.format("Fout bij het verwijderen van beelden voor accession number %s. Raadpleeg het IMS systeem voor verdere analyse.",
				uploadBeeldenPoging.getAccessionNumber());
			var logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setMelding(melding);
			logEvent.setHl7MessageStructure(bericht.getHl7Bericht());
			logEvent.setLevel(Level.WARNING);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_ERROR_ONTVANGEN, logEvent, null, client, Bevolkingsonderzoek.MAMMA);
		}
		else
		{
			baseIlmService.verwijderIlmBezwaarPoging(uploadBeeldenPoging.getUploadBeeldenVerzoek().getScreeningRonde().getDossier(), uploadBeeldenPoging.getAccessionNumber());
		}
	}

	@Override
	public List<Long> getOpenstaandeUploadVerzoekenIds()
	{
		return uploadBeeldenVerzoekRepository.findIdsByStatus(MammaUploadBeeldenVerzoekStatus.BEELDEN_GEUPLOAD);
	}

	@Override
	public void verstuurBeelden(Long uploadBeeldenVerzoekId, String sopClasses)
	{
		databaseRunner.runInSessionOnly(() ->
		{
			var succes = true;
			Set<String> errorBestanden = new HashSet<>();
			Date laatsteOnderzoeksDatum = null;
			var uploadBeeldenVerzoek = uploadBeeldenVerzoekRepository.getReferenceById(uploadBeeldenVerzoekId);

			try
			{
				var uploadBeeldenPoging = uploadBeeldenVerzoek.getLaatsteUploadPoging();
				LOG.info("Start verstuur beelden voor uploadpoging " + uploadBeeldenPoging.getId());
				var beeldenTeUploaden = valideerBeelden(uploadBeeldenPoging, errorBestanden, sopClasses);
				succes = beeldenTeUploaden != null;

				if (succes)
				{
					var accessionNumber = baseFactory.getNextUniqueMammaUitnodigingsNr();
					uploadBeeldenPoging.setAccessionNumber(accessionNumber);

					for (var uploadDocument : beeldenTeUploaden)
					{
						var dicomFile = uploadDocumentService.load(uploadDocument);
						var dicomCStoreSCU = new CStoreSCU(CStoreConfig.parse(
							preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_IMS_DICOM_CSTORE_CONFIG.toString(),
								"SIT_STORE2_SCU@localhost:11114,DICOM_Store_SCP@localhost:14843")));
						succes &= dicomCStoreSCU.store(dicomFile, sopClasses, accessionNumber,
							uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient().getPersoon().getBsn());
						var response = dicomCStoreSCU.getResponse();

						if (response != null)
						{
							var status = response.getInt(Tag.Status, -1);

							if (status != Status.Success)
							{
								succes = false;
								errorBestanden.add(uploadDocument.getNaam());
							}
							else
							{
								var attributes = dicomCStoreSCU.getFileAttributes();
								var onderzoeksDatum = attributes.getDate(Tag.StudyDate, laatsteOnderzoeksDatum);
								laatsteOnderzoeksDatum = onderzoeksDatum != null && (laatsteOnderzoeksDatum == null || onderzoeksDatum.compareTo(laatsteOnderzoeksDatum) > 0)
									? onderzoeksDatum
									: laatsteOnderzoeksDatum;
							}
						}
						else
						{
							succes = false;
						}
					}
				}
				LOG.info("Klaar met uploadpoging {}", uploadBeeldenPoging.getId());
			}
			catch (Exception e)
			{
				succes = false;
				LOG.error("Fout bij uploadpoging {}", uploadBeeldenVerzoek.getLaatsteUploadPoging().getId(), e);
			}
			finally
			{
				var finalSucces = succes;
				var finalLaatsteOnderzoeksDatum = laatsteOnderzoeksDatum;
				databaseRunner.runInNewTransaction(() ->
					afrondenUploadPoging(uploadBeeldenVerzoek, finalSucces, errorBestanden, finalLaatsteOnderzoeksDatum)
				);
			}
		});
	}

	private void afrondenUploadPoging(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, boolean succes, Set<String> errorBestanden, Date laatsteOnderzoeksDatum)
	{
		var uploadBeeldenPoging = uploadBeeldenVerzoek.getLaatsteUploadPoging();
		LOG.info("Start afronden uploadpoging {}. Succes: {}", uploadBeeldenPoging.getId(), succes);
		if (!succes)
		{
			var errorBestandNamen = String.join(", ", errorBestanden);
			uploadBeeldenVerzoek.setStatus(MammaUploadBeeldenVerzoekStatus.ERROR);
			var fouteBestandenText = (StringUtils.isNotBlank(errorBestandNamen) ? " door de volgende bestanden: " + errorBestandNamen : "");
			uploadBeeldenPoging.setStatusMelding(getErrorStatusMelding(fouteBestandenText));
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(),
				"Versturen naar Sectra is gefaald " + fouteBestandenText + ".", Bevolkingsonderzoek.MAMMA);
			if (uploadBeeldenPoging.getAccessionNumber() != null)
			{
				uitwisselportaalService.verwijderBeelden(uploadBeeldenPoging);
			}
		}
		else
		{
			uploadBeeldenVerzoek.setStatus(MammaUploadBeeldenVerzoekStatus.VERWERKT);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(),
				"Beelden zijn succesvol verstuurd naar Sectra", Bevolkingsonderzoek.MAMMA);
			berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.SCHEDULED, laatsteOnderzoeksDatum);
			berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.STARTED, null);
			berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus.COMPLETED, laatsteOnderzoeksDatum);
		}
		uploadBeeldenVerzoek.setStatusDatum(dateSupplier.getDate());

		var bestanden = uploadBeeldenPoging.getBestanden();
		bestanden.forEach(uploadDocumentService::delete);
		uploadBeeldenPoging.getBestanden().clear();

		LOG.info("Einde afronden uploadpoging {}", uploadBeeldenPoging.getId());
	}

	private List<UploadDocument> valideerBeelden(MammaUploadBeeldenPoging uploadBeeldenPoging, Set<String> errorBestanden, String sopClassConfig)
	{
		var uploadBeeldenVerzoek = uploadBeeldenPoging.getUploadBeeldenVerzoek();
		if (StringUtils.isBlank(sopClassConfig))
		{
			logUploadVerzoekGebeurtenis(uploadBeeldenVerzoek, "Foute upload DICOM: geen SOP configuratie");
			return null;
		}

		Date maxDate = null;
		var succes = true;
		try
		{
			Map<Integer, List<UploadDocument>> yearFileMap = new HashMap<>();
			for (var uploadDocument : uploadBeeldenPoging.getBestanden())
			{
				var file = uploadDocumentService.load(uploadDocument);
				try (var dicomInputStream = new DicomInputStream(file))
				{
					var attributes = dicomInputStream.readDatasetUntilPixelData();
					var date = attributes.getDate(Tag.StudyDate, null, new DatePrecision());
					if (date == null)
					{
						logUploadVerzoekGebeurtenis(uploadBeeldenVerzoek,
							"Foute upload DICOM: " + uploadDocument.getNaam() + " bevat geen onderzoeksdatum en kan niet worden geupload");
						errorBestanden.add(uploadDocument.getNaam());
						succes = false;
						continue;
					}

					var properties = new Properties();
					properties.load(new StringReader(sopClassConfig));
					var propertiesUid = new Properties();
					for (var prop : properties.stringPropertyNames())
					{
						try
						{
							var uid = UID.forName(prop);
							propertiesUid.put(uid, "");
						}
						catch (IllegalArgumentException e)
						{
							propertiesUid.put(prop, "");
						}
					}

					var fileMetaInformation = dicomInputStream.readFileMetaInformation();
					var mediaStorageSopClassUid = fileMetaInformation.getString(Tag.MediaStorageSOPClassUID);
					var sopClassUid = mediaStorageSopClassUid != null ? mediaStorageSopClassUid : attributes.getString(Tag.SOPClassUID);
					if (sopClassUid == null || propertiesUid.stringPropertyNames().stream().noneMatch(property -> property.equals(sopClassUid)))
					{
						logUploadVerzoekGebeurtenis(uploadBeeldenVerzoek, "Foute upload: bestand " + uploadDocument.getNaam() + " bevatte een ongeldige SOP class");
						errorBestanden.add(uploadDocument.getNaam());
						succes = false;
						continue;
					}

					maxDate = maxDate == null || date.compareTo(maxDate) > 0 ? date : maxDate;

					Integer year = DateUtil.toLocalDate(date).getYear();
					var filesForYear = yearFileMap.computeIfAbsent(year, k -> new ArrayList<>());
					filesForYear.add(uploadDocument);
				}
			}

			if (succes)
			{
				var screeningRonde = screeningrondeService.getLaatsteScreeningRondeMetUitslag(uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(),
					maxDate);

				var oudeScreeningRonde = uploadBeeldenVerzoek.getScreeningRonde();
				if (screeningRonde != null)
				{
					uploadBeeldenVerzoek.getScreeningRonde().getUploadBeeldenVerzoeken().remove(uploadBeeldenVerzoek);
					uploadBeeldenVerzoek.setScreeningRonde(screeningRonde);
					screeningRonde.getUploadBeeldenVerzoeken().add(uploadBeeldenVerzoek);
					hibernateService.saveOrUpdate(screeningRonde);
				}
				hibernateService.saveOrUpdateAll(uploadBeeldenVerzoek, oudeScreeningRonde);
			}

			return succes ? yearFileMap.entrySet().stream().max(Map.Entry.comparingByKey()).orElse(null).getValue() : null;
		}
		catch (Exception e)
		{
			LOG.error("Er is een technische fout opgetreden", e);
			logUploadVerzoekGebeurtenis(uploadBeeldenVerzoek, "Er is een technische fout opgetreden bij het uploaden");
			return null;
		}

	}

	private void logUploadVerzoekGebeurtenis(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, String melding)
	{
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(), melding,
			Bevolkingsonderzoek.MAMMA);
	}

	private String getErrorStatusMelding(String fouteBestandenText)
	{
		var statusMeldingPostFix = "Neem contact op met de servicedesk van het bevolkingsonderzoek";
		var statusMeldingTooLongPostfix = "... (" + statusMeldingPostFix + ").";
		var statusMelding = "Versturen naar Sectra is gefaald " + fouteBestandenText + ". " + statusMeldingPostFix + ".";
		if (statusMelding.length() > STATUS_MELDING_COLUMN_SIZE)
		{
			return statusMelding.substring(0, STATUS_MELDING_COLUMN_SIZE - statusMeldingTooLongPostfix.length()) + statusMeldingTooLongPostfix;
		}
		return statusMelding;
	}
}
