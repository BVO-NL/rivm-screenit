package nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.verwerkingstep;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.fitanalyseresultaatsetverwerking.ColonFitAnalyseResultaatSetVerwerkingConstants;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.colon.ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonFitAnalyseResultaatSetVerwerkingRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ColonFitAnalyseResultaatSetVerwerkingWriter implements ItemWriter<ColonFitAnalyseResultaat>
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ColonBaseFitService fitService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private StepExecution stepExecution;

	@Override
	public void write(Chunk<? extends ColonFitAnalyseResultaat> chunk)
	{
		var logEvent = (ColonFitAnalyseResultaatSetVerwerkingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext()
			.get(ColonFitAnalyseResultaatSetVerwerkingConstants.RAPPORTAGEKEYVERWERKING);

		var bestanden = logEvent.getRapportage().getAnalyseResultaatSets();

		ColonFitAnalyseResultaatSet resultaatSet = null;
		ColonFitAnalyseResultaatSetVerwerkingRapportageEntry verslagEntry = null;

		for (var ifobtResult : chunk.getItems())
		{
			var fitRegistratie = fitService.getFit(ifobtResult.getBarcode()).orElse(null);

			if (resultaatSet == null || !resultaatSet.equals(ifobtResult.getAnalyseResultaatSet()))
			{
				resultaatSet = ifobtResult.getAnalyseResultaatSet();
				verslagEntry = null;
			}
			if (verslagEntry == null)
			{
				for (var entry : bestanden)
				{
					if (entry.getFitAnalyseResultaatSetId().equals(resultaatSet.getId()))
					{
						verslagEntry = entry;
						break;
					}
				}
				if (verslagEntry == null)
				{
					verslagEntry = new ColonFitAnalyseResultaatSetVerwerkingRapportageEntry();
					verslagEntry.setAnalyseResultaatSetNaam(resultaatSet.getNaamBestand());
					verslagEntry.setFitAnalyseResultaatSetId(resultaatSet.getId());
					verslagEntry.setRapportage(logEvent.getRapportage());
					bestanden.add(verslagEntry);
				}
			}

			if (fitRegistratie != null && !ColonFitRegistratieStatus.NIETTEBEOORDELEN.equals(fitRegistratie.getStatus()) && fitRegistratie.getType().equals(ColonFitType.GOLD))
			{
				var client = fitRegistratie.getScreeningRonde().getDossier().getClient();

				if (fitRegistratie.getUitslag() == null)
				{
					logService.logGebeurtenis(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTAAT_VERWERKT, client, "barcode: " + fitRegistratie.getBarcode(), Bevolkingsonderzoek.COLON);
					zetAnalysegegevensOverNaarFit(resultaatSet, ifobtResult, fitRegistratie);

					if (ifobtResult.getOnbeoordeelbaarReden() != null || (!ColonFitRegistratieUtil.UITSLAG_FLAG_PRO.equals(fitRegistratie.getFlag())
						&& fitRegistratie.getFlag() != null))
					{
						fitRegistratie.setRedenNietTeBeoordelen(ifobtResult.getOnbeoordeelbaarReden());
						fitService.monsterNietBeoordeelbaar(fitRegistratie);
					}
					else
					{
						fitService.verwerkAnalyseResultaat(fitRegistratie);
					}
					verslagEntry.setAantalVerwerkingen(verslagEntry.getAantalVerwerkingen() + 1);

				}
				else
				{
					logService.logGebeurtenis(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTAAT_DUBBEL, client, "Voor FIT met barcode " + fitRegistratie.getBarcode()
						+ " is al eerder een uitslag verwerkt. Niet nogmaals verwerkt.", Bevolkingsonderzoek.COLON);
				}
			}
			else if (fitRegistratie != null && ColonFitRegistratieStatus.NIETTEBEOORDELEN.equals(fitRegistratie.getStatus()))
			{
				LOG.warn("Barcode van FIT (id: '{}') hoort bij een onbeoordeelbare FIT.", fitRegistratie.getId());
			}
			else
			{
				LOG.warn("Barcode van iFobtResult (id: '{}') is onbekend of hoort niet bij een FIT.", ifobtResult.getId());
			}
			resultaatSet.setAantalVerwerkt(resultaatSet.getAantalVerwerkt() + 1);
			if (resultaatSet.getAantalVerwerkt() >= resultaatSet.getUitslagen().size())
			{
				resultaatSet.setStatus(ColonFitAnalyseResultaatSetStatus.VERWERKT);
				if (resultaatSet.getAantalVerwerkt() > resultaatSet.getUitslagen().size())
				{
					LOG.warn("Aantal verwerkt is groter uitslagen {} {}", resultaatSet.getAantalVerwerkt(), resultaatSet.getUitslagen().size());
				}
			}

			hibernateService.saveOrUpdate(resultaatSet);
		}
	}

	private void zetAnalysegegevensOverNaarFit(ColonFitAnalyseResultaatSet resultaatSet, ColonFitAnalyseResultaat ifobtResult, ColonFitRegistratie fitRegistratie)
	{
		fitRegistratie.setAnalyseDatum(ifobtResult.getAnalyseDatum());
		fitRegistratie.setVerwerkingsDatum(currentDateSupplier.getDate());
		fitRegistratie.setFlag(ifobtResult.getFlag());

		if (ColonFitRegistratieUtil.UITSLAG_FLAG_PRO.equals(fitRegistratie.getFlag()))
		{
			fitRegistratie.setGeinterpreteerdeUitslag(ColonGeinterpreteerdeUitslag.ONGUNSTIG);
		}
		if (correctForInpakcentrumIncident(fitRegistratie))
		{
			var uitslag = ifobtResult.getUitslag();
			fitRegistratie.setUitslag(uitslag);
		}

		fitRegistratie.setFitLaboratorium(resultaatSet.getLaboratorium());
		fitRegistratie.setInstrumentId(ifobtResult.getInstrumentId());
	}

	private boolean correctForInpakcentrumIncident(ColonFitRegistratie fitRegistratie)
	{
		var colonUitnodiging = fitRegistratie.getUitnodiging();
		if (colonUitnodiging != null)
		{
			String trackTraceId = colonUitnodiging.getTrackTraceId();
			if (trackTraceId != null && (trackTraceId.startsWith("16859/") || trackTraceId.startsWith("17531/")))
			{
				LOG.info("FIT (id: '{}') is aangepast naar status verwijderd (Incident Inpakcentrum)", fitRegistratie.getId());
				fitRegistratie.setStatus(ColonFitRegistratieStatus.VERWIJDERD);
				fitRegistratie.setStatusDatum(currentDateSupplier.getDate());
				return false;
			}
		}
		return true;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
