package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelstep;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import jakarta.persistence.NonUniqueResultException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.IfobtKoppelenConstants;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.IfobtKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.repository.colon.ColonScreeningRondeRepository;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.colon.ColonBaseFITService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import generated.KOPPELDATA.VERZONDENUITNODIGING;
import generated.KOPPELDATA.VERZONDENUITNODIGING.MATCHINGFIELDS.MATCHINGFIELD;

import static nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification.heeftUitnodigingId;

@Component
@Slf4j
@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
public class IFobtKoppelWriter implements ItemWriter<VERZONDENUITNODIGING>
{
	@Autowired
	private ColonBaseFITService fitService;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

	@Autowired
	private ColonScreeningRondeRepository screeningRondeRepository;

	private StepExecution stepExecution;

	private IfobtKoppelingBeeindigdLogEvent logEvent;

	@Override
	public void write(Chunk<? extends VERZONDENUITNODIGING> chunk) throws Exception
	{
		logEvent = (IfobtKoppelingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext().get(IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN);

		var verzendDatumFormat = new SimpleDateFormat("dd-MM-yyyy");
		for (VERZONDENUITNODIGING verzondenUitnodiging : chunk.getItems())
		{
			String ifobtBarcodeGold = null;
			String ifobtBarcodeExtra = null;
			String trackTraceId = null;
			try
			{
				var uitnodiging = uitnodigingRepository.findOne(heeftUitnodigingId(verzondenUitnodiging.getID())).orElse(null);

				ColonOnderzoeksVariant onderzoeksVariant = null;
				if (uitnodiging != null)
				{
					onderzoeksVariant = uitnodiging.getOnderzoeksVariant();
				}
				boolean barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.GOLD);
				boolean barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE);

				ifobtBarcodeGold = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_GOLD, barcodeGoldVerplicht);
				ifobtBarcodeExtra = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA, barcodeExtraVerplicht);

				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				uitnodiging.setTrackTraceId(trackTraceId);
				Date datumVerstuurd = verzendDatumFormat.parse(getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_DATUM_VERZENDING, true));

				var screeningRonde = uitnodiging.getScreeningRonde();

				fitService.koppelTestIndienMogelijk(ifobtBarcodeGold, IFOBTType.GOLD, uitnodiging, datumVerstuurd, screeningRonde);
				fitService.koppelTestIndienMogelijk(ifobtBarcodeExtra, IFOBTType.STUDIE, uitnodiging, datumVerstuurd, screeningRonde);

				uitnodigingRepository.save(uitnodiging);
				screeningRondeRepository.save(screeningRonde);

				logEvent.setAantalIfobtenVerwerkt(logEvent.getAantalIfobtenVerwerkt() + 1);
			}
			catch (ParseException | NonUniqueResultException e)
			{
				LOG.error("Fout bij verwerken van koppel data regel ", e);
				var melding = String.format(KoppelConstants.COLON_ONBEKENDE_FOUT, verzondenUitnodiging.getID(),
					StringUtils.defaultIfBlank(ifobtBarcodeGold, KoppelConstants.DEFAULT_STRING),
					StringUtils.defaultIfBlank(ifobtBarcodeExtra, KoppelConstants.DEFAULT_STRING), e.getMessage());
				logEvent.setLevel(Level.ERROR);
				logEvent.setMelding(melding);
				throw new IllegalStateException(melding);
			}
		}
	}

	private String getMatchingFieldValue(VERZONDENUITNODIGING verzondenUitnodiging, String matchingFieldName, boolean required)
	{
		for (MATCHINGFIELD matchingField : verzondenUitnodiging.getMATCHINGFIELDS().getMATCHINGFIELD())
		{
			if (matchingField.getNAME().equalsIgnoreCase(matchingFieldName))
			{
				return matchingField.getVALUE();
			}
		}

		if (required)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + matchingFieldName);
			throw new IllegalStateException("MatchingField not found: " + matchingFieldName);
		}
		return null;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
