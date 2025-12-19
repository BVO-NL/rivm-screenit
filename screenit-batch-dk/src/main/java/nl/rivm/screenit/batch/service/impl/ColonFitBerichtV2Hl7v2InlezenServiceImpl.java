package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.ColonFitBerichtHl7v2InlezenService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatDto;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.berichten.ColonFitAnalyseResultatenBericht;
import nl.rivm.screenit.model.colon.berichten.ColonHl7BerichtToFitAnalyseResultaatSetWrapper;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.colon.ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonFitAnalyseResultaatSetVerwerkingRapportage;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonFitAnalyseResultaatSetVerwerkingRapportageEntry;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatRepository;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetRepository;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEventRepository;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetVerwerkingRapportageEntryRepository;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetVerwerkingRapportageRepository;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultatenBerichtRepository;
import nl.rivm.screenit.repository.colon.ColonFitLaboratoriumRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLControleBarcodeRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;

import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.parser.Parser;

import static nl.rivm.screenit.util.colon.ColonFitRegistratieUtil.ANALYSE_RESULTAAT_FLAG_PRO;

@Slf4j
@Service
@RequiredArgsConstructor
public class ColonFitBerichtV2Hl7v2InlezenServiceImpl implements ColonFitBerichtHl7v2InlezenService
{
	private static final String QC_BARCODE_PREFIX = "QC";

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonFitLaboratoriumRepository fitLaboratoriumRepository;

	private final ColonFitAnalyseResultatenBerichtRepository fitAnalyseResultatenBerichtRepository;

	private final ColonSKMLControleBarcodeRepository skmlControleBarcodeRepository;

	private final ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEventRepository fitVerwerkingBeeindigdLogEventRepository;

	private final ColonFitAnalyseResultaatSetVerwerkingRapportageRepository fitVerwerkingRapportageRepository;

	private final ColonFitAnalyseResultaatSetVerwerkingRapportageEntryRepository fitVerwerkingRapportageEntryRepository;

	private final ColonFitAnalyseResultaatRepository fitAnalyseResultaatRepository;

	private final ColonFitAnalyseResultaatSetRepository fitAnalyseResultaatSetRepository;

	private final OrganisatieService organisatieService;

	private final LogService logService;

	private final ColonBaseFitService fitService;

	@Override
	public List<ColonFitAnalyseResultatenBericht> getAlleNietVerwerkteFitBerichten()
	{
		return fitAnalyseResultatenBerichtRepository.findByStatus(BerichtStatus.NIEUW);
	}

	@Override
	@Transactional
	public void verwerkOntvangenFitBericht(ColonFitAnalyseResultatenBericht bericht)
	{
		var verwerkingLogEvent = new ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent();
		var rapportage = new ColonFitAnalyseResultaatSetVerwerkingRapportage();
		verwerkingLogEvent.setRapportage(rapportage);
		fitVerwerkingRapportageRepository.save(rapportage);
		fitVerwerkingBeeindigdLogEventRepository.save(verwerkingLogEvent);

		try
		{
			var hapiMsg = transformToMessage(bericht.getHl7Bericht());

			var wrapper = new ColonHl7BerichtToFitAnalyseResultaatSetWrapper((OUL_R22) hapiMsg);
			var verslagEntry = new ColonFitAnalyseResultaatSetVerwerkingRapportageEntry();

			rapportage.setDatumVerwerking(currentDateSupplier.getDate());
			verslagEntry.setAnalyseResultaatSetNaam(bericht.getMessageId());
			verslagEntry.setRapportage(rapportage);

			var berichtVerwerkenMelding = "Start met ontvangen van uitslagen in FIT HL7 bericht: " + bericht.getMessageId();
			LOG.info(berichtVerwerkenMelding);
			logService.logGebeurtenis(LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_OPSLAAN_GESTART, (Account) null, null, berichtVerwerkenMelding, Bevolkingsonderzoek.COLON);

			verwerkFitResults(wrapper, verslagEntry, bericht, verwerkingLogEvent);

			bericht.setStatusDatum(currentDateSupplier.getDate());
			bericht.setStatus(BerichtStatus.VERWERKT);

			fitVerwerkingRapportageRepository.save(rapportage);
			fitAnalyseResultatenBerichtRepository.save(bericht);
			fitVerwerkingBeeindigdLogEventRepository.save(verwerkingLogEvent);
			logService.logGebeurtenis(LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_OPSLAAN_AFGEROND, verwerkingLogEvent, Bevolkingsonderzoek.COLON);
		}
		catch (Exception e)
		{
			LOG.warn("Fout bij verwerking van HL7 bericht", e);
			logError(bericht, e.getMessage(), verwerkingLogEvent);
		}
	}

	private void verwerkFitResults(ColonHl7BerichtToFitAnalyseResultaatSetWrapper wrapper, ColonFitAnalyseResultaatSetVerwerkingRapportageEntry verslagEntry,
		ColonFitAnalyseResultatenBericht bericht,
		ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		ColonFitAnalyseResultaatSet resultaatSet = null;

		for (var result : wrapper.getResults())
		{
			try
			{
				var barcode = result.getBarcode();
				LOG.info("Uitslag voor barcode verwerken: {}", barcode);
				var resultaatType = bepaalResultaatType(barcode);
				resultaatSet = createOrGetResultaatSet(result);
				verslagEntry.setFitAnalyseResultaatSetId(resultaatSet.getId());

				if (resultaatType != null)
				{
					var resultaat = new ColonFitAnalyseResultaat();
					resultaat.setAnalyseDatum(result.getDateTimeResult());
					resultaat.setBarcode(barcode);
					resultaat.setType(resultaatType);
					resultaat.setInstrumentId(result.getInstrumentID());
					fitUitslagVerwerking(result, resultaat);
					resultaat.setAnalyseResultaatSet(resultaatSet);
					fitAnalyseResultaatRepository.save(resultaat);

					if (ColonFitAnalyseResultaatType.CLIENT != resultaatType)
					{
						resultaatSet.setAantalControleUitslagen(resultaatSet.getAantalControleUitslagen() + 1);
						logQCTestOntvangen(resultaatType);
					}
				}
				else
				{
					var isVerwijderdeBarcode = fitService.isVerwijderdeBarcode(barcode);
					var melding = String.format("FIT of controle buis met barcode %s in bericht %s bestaat niet%s.", barcode, result.getBestandsNaam(),
						isVerwijderdeBarcode ? " meer" : "");
					logService.logGebeurtenis(LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_OPSLAAN_ONBEKENDE_BARCODE, melding, Bevolkingsonderzoek.COLON);
					LOG.warn(melding);
				}
				verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);
				fitVerwerkingRapportageEntryRepository.save(verslagEntry);
				fitAnalyseResultaatSetRepository.save(resultaatSet);

			}
			catch (Exception e)
			{
				var melding = "Onbekende fout in regel met barcode " + result.getBarcode() + "\n";
				melding += e.getMessage();

				LOG.warn("Fout bij verwerking van uitslag regel ", e);
				logWarning(bericht, melding, verwerkingLogEvent);
			}
		}
		if (resultaatSet != null)
		{
			resultaatSet.setStatus(bepaalBestandStatus());
		}

	}

	private @Nullable ColonFitAnalyseResultaatType bepaalResultaatType(String barcode)
	{
		var fit = fitService.getFit(barcode).orElse(null);
		ColonFitAnalyseResultaatType resultaatType = null;
		if (fit == null || fit.getType() == ColonFitType.STUDIE)
		{
			if (fitService.isDk2026Actief())
			{
				if (barcode.startsWith(QC_BARCODE_PREFIX))
				{
					resultaatType = ColonFitAnalyseResultaatType.QC;
				}
			}
			else
			{
				var skmlBarcode = skmlControleBarcodeRepository.findByBarcode(barcode);
				if (skmlBarcode.isPresent())
				{
					resultaatType = skmlBarcode.get().getType();
				}
			}
		}
		else
		{
			resultaatType = ColonFitAnalyseResultaatType.CLIENT;
			logAction(fit);
		}
		return resultaatType;
	}

	private void fitUitslagVerwerking(ColonFitAnalyseResultaatDto result, ColonFitAnalyseResultaat uitslag)
	{
		uitslag.setFlag(result.getFlag());

		if (Objects.equals(result.getFlag(), ANALYSE_RESULTAAT_FLAG_PRO))
		{

			uitslag.setUitslag(null);
		}
		else if (result.getFlag() != null || result.getOnbeoordeelbaarReden() != null)
		{

			uitslag.setAnalyseDatum(currentDateSupplier.getDate());

			var reden = RedenNietTeBeoordelen.bepaalReden(result.getOnbeoordeelbaarReden());
			uitslag.setOnbeoordeelbaarReden(reden);
		}
		else
		{

			uitslag.setUitslag(new BigDecimal(result.getResultValue()));
		}
	}

	private Message transformToMessage(String bericht) throws HL7Exception, IOException
	{
		try (HapiContext context = new DefaultHapiContext())
		{

			context.getExecutorService();
			Parser p = context.getPipeParser();
			return p.parse(bericht);
		}
	}

	private void logQCTestOntvangen(ColonFitAnalyseResultaatType resultaatType)
	{
		var logGebeurtenis = resultaatType.getLogGebeurtenis();
		if (logGebeurtenis != null)
		{
			logService.logGebeurtenis(logGebeurtenis, (Account) null, Bevolkingsonderzoek.COLON);
		}
	}

	private ColonFitAnalyseResultaatSet createOrGetResultaatSet(ColonFitAnalyseResultaatDto result)
	{
		return fitService.getFitAnalyseResultaatSet(result.getBestandsNaam()).orElseGet(() ->
		{
			var resultaatSet = new ColonFitAnalyseResultaatSet();
			resultaatSet.setStatus(ColonFitAnalyseResultaatSetStatus.NIEUW);
			resultaatSet.setStatusDatum(currentDateSupplier.getDate());
			var labID = result.getLabID();
			var iFobtLaboratorium = fitLaboratoriumRepository.findByLabId(labID);
			if (iFobtLaboratorium == null)
			{
				var melding = "FIT laboratorium met id " + labID + " in bestand " + result.getBestandsNaam() + " bestaat niet (bestand overgeslagen).";
				throw new IllegalStateException(melding);
			}
			resultaatSet.setLaboratorium(iFobtLaboratorium);

			resultaatSet.setNaamBestand(result.getBestandsNaam());
			resultaatSet.setPathBestand("");

			fitAnalyseResultaatSetRepository.save(resultaatSet);

			return resultaatSet;
		});
	}

	private @NotNull ColonFitAnalyseResultaatSetStatus bepaalBestandStatus()
	{
		return fitService.isDk2026Actief() ? ColonFitAnalyseResultaatSetStatus.GEAUTORISEERD : ColonFitAnalyseResultaatSetStatus.INGELEZEN;
	}

	private void logAction(ColonFitRegistratie fit)
	{
		var client = ColonFitRegistratieUtil.getUitnodiging(fit).getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTAAT_ONTVANGEN, client, "barcode: " + fit.getBarcode(), Bevolkingsonderzoek.COLON);
	}

	private void logWarning(ColonFitAnalyseResultatenBericht ontvangenBericht, String message, ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		if (!BerichtStatus.FOUT.equals(ontvangenBericht.getStatus()))
		{
			ontvangenBericht.setStatus(BerichtStatus.WAARSCHUWING);
		}
		fitAnalyseResultatenBerichtRepository.save(ontvangenBericht);
		var melding = "Uitslag in FIT HL7 bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		addMelding(verwerkingLogEvent, melding);
	}

	private void addMelding(ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent verwerkingLogEvent, String melding)
	{
		var huidigeMelding = verwerkingLogEvent.getMelding();
		if (melding == null)
		{
			melding = "Er is een onbekende fout opgetreden bij de verwerking van een uitslag. Neem contact op met de helpdesk.";
		}
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;
		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		verwerkingLogEvent.setMelding(huidigeMelding);
	}

	@Override
	@Transactional
	public void logError(ColonFitAnalyseResultatenBericht ontvangenBericht, String message, ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		ontvangenBericht.setStatus(BerichtStatus.FOUT);
		ontvangenBericht.setStatusDatum(currentDateSupplier.getDate());
		fitAnalyseResultatenBerichtRepository.save(ontvangenBericht);
		var laboratorium = ontvangenBericht.getLaboratorium();
		var melding = "FIT HL7 Bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		logging(LogGebeurtenis.COLON_JOB_FIT_ANALYSE_RESULTATEN_OPSLAAN_AFGEROND, laboratorium, Level.ERROR, melding);
		if (verwerkingLogEvent != null)
		{
			addMelding(verwerkingLogEvent, melding);
		}
	}

	private String logging(LogGebeurtenis gebeurtenis, ColonFitLaboratorium laboratorium, Level level, String melding)
	{
		var event = new LogEvent();
		event.setLevel(level);
		event.setMelding(melding);
		List<Organisatie> organisaties = new ArrayList<>(organisatieService.getActieveOrganisaties(Rivm.class));
		if (laboratorium != null)
		{
			melding += " Laboratorium: " + laboratorium.getNaam();
		}
		logService.logGebeurtenis(gebeurtenis, organisaties, event, Bevolkingsonderzoek.COLON);
		return melding;
	}
}
