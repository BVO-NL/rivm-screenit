package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelmetreststep;

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

import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.NonUniqueResultException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.IfobtKoppelenConstants;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.IfobtKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.repository.colon.ColonScreeningRondeRepository;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification.heeftUitnodigingId;

@Component
@Slf4j
public class FitKoppelMetRestWriter implements ItemWriter<InpakcentrumKoppelDataDto>
{
	@Autowired
	private ColonBaseFITService fitService;

	private StepExecution stepExecution;

	private IfobtKoppelingBeeindigdLogEvent logEvent;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

	@Autowired
	private ColonScreeningRondeRepository screeningRondeRepository;

	@Override
	public void write(Chunk<? extends InpakcentrumKoppelDataDto> chunk) throws Exception
	{
		logEvent = (IfobtKoppelingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext().get(IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN);

		for (var item : chunk.getItems())
		{
			String fitBarcodeGold = null;
			String fitBarcodeExtra = null;

			try
			{
				var uitnodiging = uitnodigingRepository.findOne(heeftUitnodigingId(item.getId()))
					.orElseThrow(() -> new EntityNotFoundException("Uitnodiging niet gevonden voor ID: " + item.getId()));

				var onderzoeksVariant = uitnodiging.getOnderzoeksVariant();
				validate(item, onderzoeksVariant);

				fitBarcodeGold = item.getBarcode();
				fitBarcodeExtra = item.getBarcodeExtra();

				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				var datumVerstuurd = DateUtil.parseDateForPattern(item.getDatumVerzending(), Constants.DEFAULT_DATE_FORMAT);
				var screeningRonde = uitnodiging.getScreeningRonde();

				fitService.koppelTestIndienMogelijk(fitBarcodeGold, IFOBTType.GOLD, uitnodiging, datumVerstuurd, screeningRonde);
				fitService.koppelTestIndienMogelijk(fitBarcodeExtra, IFOBTType.STUDIE, uitnodiging, datumVerstuurd, screeningRonde);

				uitnodigingRepository.save(uitnodiging);
				screeningRondeRepository.save(screeningRonde);

				logEvent.setAantalIfobtenVerwerkt(logEvent.getAantalIfobtenVerwerkt() + 1);
			}
			catch (NonUniqueResultException | EntityNotFoundException e)
			{
				LOG.error("Fout bij verwerken van koppel data regel ", e);
				var melding = String.format(KoppelConstants.COLON_ONBEKENDE_FOUT, item.getId(), StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING),
					StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), e.getMessage());
				logEvent.setLevel(Level.ERROR);
				logEvent.setMelding(melding);
				throw new IllegalStateException(melding);
			}
		}
	}

	private void validate(InpakcentrumKoppelDataDto item, ColonOnderzoeksVariant onderzoeksvariant)
	{
		var fitBarcodeGold = item.getBarcode();
		var barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksvariant, IFOBTType.GOLD);
		if (fitBarcodeGold == null && barcodeGoldVerplicht)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + KoppelConstants.KOPPEL_BARCODE);
			throw new IllegalStateException("MatchingField not found: " + KoppelConstants.KOPPEL_BARCODE);
		}

		var fitBarcodeExtra = item.getBarcodeExtra();
		var barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksvariant, IFOBTType.STUDIE);
		if (fitBarcodeExtra == null && barcodeExtraVerplicht)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA);
			throw new IllegalStateException("MatchingField not found: " + KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA);
		}
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
