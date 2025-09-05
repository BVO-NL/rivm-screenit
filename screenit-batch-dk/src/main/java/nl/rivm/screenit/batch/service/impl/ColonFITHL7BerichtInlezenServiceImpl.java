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

import nl.rivm.screenit.batch.service.ColonFITHL7BerichtInlezenService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTResult;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtHL7BerichtWrapper;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportage;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportageEntry;
import nl.rivm.screenit.repository.colon.ColonFITBestandRepository;
import nl.rivm.screenit.repository.colon.ColonFITLaboratoriumRepository;
import nl.rivm.screenit.repository.colon.ColonFITUitslagBerichtRepository;
import nl.rivm.screenit.repository.colon.ColonFITUitslagRepository;
import nl.rivm.screenit.repository.colon.ColonFITVerwerkingBeeindigdLogEventRepository;
import nl.rivm.screenit.repository.colon.ColonFITVerwerkingRapportageEntryRepository;
import nl.rivm.screenit.repository.colon.ColonFITVerwerkingRapportageRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLControleBarcodeRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.util.FITTestUtil;

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

@Slf4j
@Service
@RequiredArgsConstructor
public class ColonFITHL7BerichtInlezenServiceImpl implements ColonFITHL7BerichtInlezenService
{
	private static final String QC_BARCODE_PREFIX = "QC";

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonFITLaboratoriumRepository fitLaboratoriumRepository;

	private final ColonFITUitslagBerichtRepository fitUitslagBerichtRepository;

	private final ColonSKMLControleBarcodeRepository skmlControleBarcodeRepository;

	private final ColonFITVerwerkingBeeindigdLogEventRepository fitVerwerkingBeeindigdLogEventRepository;

	private final ColonFITVerwerkingRapportageRepository fitVerwerkingRapportageRepository;

	private final ColonFITVerwerkingRapportageEntryRepository fitVerwerkingRapportageEntryRepository;

	private final ColonFITUitslagRepository fitUitslagRepository;

	private final ColonFITBestandRepository fitBestandRepository;

	private final OrganisatieService organisatieService;

	private final LogService logService;

	private final ColonBaseFITService fitService;

	@Override
	public List<ColonIFobtUitslagBericht> getAlleNietVerwerkteFitBerichten()
	{
		return fitUitslagBerichtRepository.findByStatus(BerichtStatus.NIEUW);
	}

	@Override
	@Transactional
	public void verwerkOntvangenFitBericht(ColonIFobtUitslagBericht bericht)
	{
		var verwerkingLogEvent = new IfobtVerwerkingBeeindigdLogEvent();
		var rapportage = new IfobtVerwerkingRapportage();
		verwerkingLogEvent.setRapportage(rapportage);
		fitVerwerkingRapportageRepository.save(rapportage);
		fitVerwerkingBeeindigdLogEventRepository.save(verwerkingLogEvent);

		try
		{
			var hapiMsg = transformToMessage(bericht.getHl7Bericht());

			var wrapper = new ColonIFobtHL7BerichtWrapper((OUL_R22) hapiMsg);
			var verslagEntry = new IfobtVerwerkingRapportageEntry();

			rapportage.setDatumVerwerking(currentDateSupplier.getDate());
			verslagEntry.setBestandsNaam(bericht.getMessageId());
			verslagEntry.setRapportage(rapportage);

			var berichtVerwerkenMelding = "Start met ontvangen van uitslagen in FIT HL7 bericht: " + bericht.getMessageId();
			LOG.info(berichtVerwerkenMelding);
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_INLEZEN_GESTART, (Account) null, null, berichtVerwerkenMelding, Bevolkingsonderzoek.COLON);

			verwerkFITResults(wrapper, verslagEntry, bericht, verwerkingLogEvent);

			bericht.setStatusDatum(currentDateSupplier.getDate());
			bericht.setStatus(BerichtStatus.VERWERKT);

			fitVerwerkingRapportageRepository.save(rapportage);
			fitUitslagBerichtRepository.save(bericht);
			fitVerwerkingBeeindigdLogEventRepository.save(verwerkingLogEvent);
			logService.logGebeurtenis(LogGebeurtenis.IFOBT_INLEZEN_AFGEROND, verwerkingLogEvent, Bevolkingsonderzoek.COLON);
		}
		catch (Exception e)
		{
			LOG.warn("Fout bij verwerking van HL7 bericht", e);
			logError(bericht, e.getMessage(), verwerkingLogEvent);
		}
	}

	private void verwerkFITResults(ColonIFobtHL7BerichtWrapper wrapper, IfobtVerwerkingRapportageEntry verslagEntry, ColonIFobtUitslagBericht bericht,
		IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		IFOBTBestand bestand = null;

		for (var result : wrapper.getResults())
		{
			try
			{
				var barcode = result.getSid();
				LOG.info("Uitslag voor barcode verwerken: {}", barcode);
				var uitslagType = bepaalUitslagType(barcode);
				bestand = createOrGetBestand(result);
				verslagEntry.setIfobtBestandId(bestand.getId());

				if (uitslagType != null)
				{
					var uitslag = new IFOBTUitslag();
					uitslag.setAnalyseDatum(result.getDateTimeResult());
					uitslag.setBarcode(barcode);
					uitslag.setType(uitslagType);
					fitUitslagVerwerking(result, uitslag);
					uitslag.setBestand(bestand);
					fitUitslagRepository.save(uitslag);

					if (IFOBTUitslagType.CLIENT != uitslagType)
					{
						bestand.setAantalControleUitslagen(bestand.getAantalControleUitslagen() + 1);
						logQCTestOntvangen(uitslagType);
					}
				}
				else
				{
					var isVerwijderdeBarcode = fitService.isVerwijderdeBarcode(barcode);
					var melding = String.format("FIT of controle buis met barcode %s in bericht %s bestaat niet%s.", barcode, result.getBestandsNaam(),
						isVerwijderdeBarcode ? " meer" : "");
					logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONBEKENDE_BARCODE, melding, Bevolkingsonderzoek.COLON);
					LOG.warn(melding);
				}
				verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);
				fitVerwerkingRapportageEntryRepository.save(verslagEntry);
				fitBestandRepository.save(bestand);

			}
			catch (Exception e)
			{
				var melding = "Onbekende fout in regel met barcode " + result.getSid() + "\n";
				melding += e.getMessage();

				LOG.warn("Fout bij verwerking van uitslag regel ", e);
				logWarning(bericht, melding, verwerkingLogEvent);
			}
		}
		if (bestand != null)
		{
			bestand.setStatus(bepaalBestandStatus());
		}

	}

	private @Nullable IFOBTUitslagType bepaalUitslagType(String barcode)
	{
		var fit = fitService.getFit(barcode).orElse(null);
		IFOBTUitslagType uitslagType = null;
		if (fit == null || fit.getType() == IFOBTType.STUDIE)
		{
			if (fitService.isDk2026Actief())
			{
				if (barcode.startsWith(QC_BARCODE_PREFIX))
				{
					uitslagType = IFOBTUitslagType.QC;
				}
			}
			else
			{
				var skmlBarcode = skmlControleBarcodeRepository.findByBarcode(barcode);
				if (skmlBarcode.isPresent())
				{
					uitslagType = skmlBarcode.get().getType();
				}
			}
		}
		else
		{
			uitslagType = IFOBTUitslagType.CLIENT;
			logAction(fit);
		}
		return uitslagType;
	}

	private void fitUitslagVerwerking(IFOBTResult result, IFOBTUitslag uitslag)
	{
		uitslag.setFlag(result.getFlag());

		if (Objects.equals(result.getFlag(), FITTestUtil.UITSLAG_FLAG_PRO))
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

	private void logQCTestOntvangen(IFOBTUitslagType uitslagType)
	{
		var logGebeurtenis = uitslagType.getLogGebeurtenis();
		if (logGebeurtenis != null)
		{
			logService.logGebeurtenis(logGebeurtenis, (Account) null, Bevolkingsonderzoek.COLON);
		}
	}

	private IFOBTBestand createOrGetBestand(IFOBTResult result)
	{
		return fitService.getIfobtBestand(result.getBestandsNaam()).orElseGet(() ->
		{
			var bestand = new IFOBTBestand();
			bestand.setStatus(IFOBTBestandStatus.NIEUW);
			bestand.setStatusDatum(currentDateSupplier.getDate());
			bestand.setInstumentId(result.getInstrumentID());
			var labID = result.getLabID();
			var iFobtLaboratorium = fitLaboratoriumRepository.findByLabId(labID);
			if (iFobtLaboratorium == null)
			{
				var melding = "FIT laboratorium met id " + labID + " in bestand " + result.getBestandsNaam() + " bestaat niet (bestand overgeslagen).";
				throw new IllegalStateException(melding);
			}
			bestand.setLaboratorium(iFobtLaboratorium);

			bestand.setNaamBestand(result.getBestandsNaam());
			bestand.setPathBestand("");

			fitBestandRepository.save(bestand);

			return bestand;
		});
	}

	private @NotNull IFOBTBestandStatus bepaalBestandStatus()
	{
		return fitService.isDk2026Actief() ? IFOBTBestandStatus.GEAUTORISEERD : IFOBTBestandStatus.INGELEZEN;
	}

	private void logAction(IFOBTTest fit)
	{
		var client = FITTestUtil.getUitnodiging(fit).getScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_ONTVANGEN, client, "barcode: " + fit.getBarcode(), Bevolkingsonderzoek.COLON);
	}

	private void logWarning(ColonIFobtUitslagBericht ontvangenBericht, String message, IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		if (!BerichtStatus.FOUT.equals(ontvangenBericht.getStatus()))
		{
			ontvangenBericht.setStatus(BerichtStatus.WAARSCHUWING);
		}
		fitUitslagBerichtRepository.save(ontvangenBericht);
		var melding = "Uitslag in FIT HL7 bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		addMelding(verwerkingLogEvent, melding);
	}

	private void addMelding(IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent, String melding)
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
	public void logError(ColonIFobtUitslagBericht ontvangenBericht, String message, IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent)
	{
		ontvangenBericht.setStatus(BerichtStatus.FOUT);
		ontvangenBericht.setStatusDatum(currentDateSupplier.getDate());
		fitUitslagBerichtRepository.save(ontvangenBericht);
		var laboratorium = ontvangenBericht.getLaboratorium();
		var melding = "FIT HL7 Bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. (" + message + ")";
		logging(LogGebeurtenis.IFOBT_INLEZEN_AFGEROND, laboratorium, Level.ERROR, melding);
		if (verwerkingLogEvent != null)
		{
			addMelding(verwerkingLogEvent, melding);
		}
	}

	private String logging(LogGebeurtenis gebeurtenis, IFobtLaboratorium laboratorium, Level level, String melding)
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
