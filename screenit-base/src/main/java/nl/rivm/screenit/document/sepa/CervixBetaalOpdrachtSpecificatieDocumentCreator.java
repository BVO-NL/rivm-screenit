package nl.rivm.screenit.document.sepa;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;

import com.aspose.words.Document;
import com.aspose.words.MailMergeCleanupOptions;
import com.aspose.words.net.System.Data.DataSet;
import com.aspose.words.net.System.Data.DataTable;

@Slf4j
public class CervixBetaalOpdrachtSpecificatieDocumentCreator extends BaseDocumentCreator
{

	private List<CervixBetaalopdrachtRegel> labRegels = new ArrayList<>();

	private List<CervixBetaalopdrachtRegel> huisartsRegels = new ArrayList<>();

	private DataSet betaalOpdracht = new DataSet();

	private static String FIELD_BETALINGSKENMERK = "betalingskenmerk";

	private static String TABLE_LABS = "labs";

	private static String FIELD_TOTAAL = "totaal";

	private static String FIELD_NAAM = "naam";

	private static String TABLE_LAB_SPECIFICATIE = "labspecificatie";

	private static String TABLE_HA_SPECIFICATIE = "haspecificatie";

	private static String FIELD_VERRICHTING = "verrichting";

	private static String FIELD_IBAN = "iban";

	private static String FIELD_TENAAMSTELLING = "tenaamstelling";

	private static String FIELD_BEDRAG = "bedrag";

	private static String FIELD_AANTAL = "aantal";

	private static String FIELD_SUBTOTAAL = "subtotaal";

	private static String ID = "_id";

	private class SplittedSpecificatie
	{
		int aantal = 0;

		BigDecimal tarief = BigDecimal.ZERO;

		BigDecimal totaal = BigDecimal.ZERO;

	}

	public CervixBetaalOpdrachtSpecificatieDocumentCreator(CervixBetaalopdracht opdracht)
	{
		var cervixBetaalopdrachtRegels = opdracht.getBetaalopdrachtRegels().stream()
			.sorted(Comparator.comparing(CervixBetaalopdrachtRegel::getNaarTenaamstelling)).collect(Collectors.toList());
		for (var opdrachtRegel : cervixBetaalopdrachtRegels)
		{
			if (opdrachtRegel.getLaboratorium() != null)
			{
				labRegels.add(opdrachtRegel);
			}
			else
			{
				huisartsRegels.add(opdrachtRegel);
			}
		}

		try
		{
			initDataSet();
		}
		catch (Exception e)
		{
			LOG.error("Er is iets fout gegaan met het aanmaken.", e);
		}
	}

	private void initDataSet()
	{
		createLabTabel();
		insertRegelsEnSpecificaties(labRegels, TABLE_LABS, TABLE_LAB_SPECIFICATIE);
		createHuisartsTabel();
		insertSpecificatiesHuisartsen(huisartsRegels, TABLE_HA_SPECIFICATIE);
	}

	private DataTable createLabTabel()
	{
		var labTable = getOrCreateDataTable(betaalOpdracht, TABLE_LABS, TABLE_LABS + ID, FIELD_TOTAAL, FIELD_NAAM, FIELD_IBAN);
		String[] columns = {
			TABLE_LAB_SPECIFICATIE + ID,
			TABLE_LABS,
			FIELD_VERRICHTING,
			FIELD_IBAN,
			FIELD_TENAAMSTELLING,
			FIELD_BEDRAG,
			FIELD_AANTAL,
			FIELD_SUBTOTAAL,
			FIELD_BETALINGSKENMERK
		};
		var labSpecTable = getOrCreateDataTable(betaalOpdracht, TABLE_LAB_SPECIFICATIE, columns);
		betaalOpdracht.getRelations().add(labTable, labSpecTable, TABLE_LABS + ID, TABLE_LABS);
		return labTable;
	}

	private DataTable createHuisartsTabel()
	{
		String[] columns = {
			TABLE_HA_SPECIFICATIE + ID,
			FIELD_VERRICHTING,
			FIELD_IBAN,
			FIELD_TENAAMSTELLING,
			FIELD_BEDRAG,
			FIELD_AANTAL,
			FIELD_SUBTOTAAL,
			FIELD_BETALINGSKENMERK
		};
		return getOrCreateDataTable(betaalOpdracht, TABLE_HA_SPECIFICATIE, columns);
	}

	public void insertSpecificatiesHuisartsen(List<CervixBetaalopdrachtRegel> regels, String tableSpecificatieNaam)
	{
		var tableSpecificatie = getDataTable(betaalOpdracht, tableSpecificatieNaam);
		for (var regel : regels)
		{
			for (var spec : regel.getSpecificaties())
			{
				for (var specificatie : bepaalSpecificaties(spec))
				{
					insertRow(tableSpecificatie, getNextSequence(), spec.getTariefType().getNaam(), regel.getNaarIban(), regel.getNaarTenaamstelling(),
						NumberFormat.getCurrencyInstance().format(specificatie.tarief), specificatie.aantal,
						NumberFormat.getCurrencyInstance().format(specificatie.totaal), regel.getBetaalopdracht().getBetalingskenmerk());
				}
			}
		}
	}

	private Collection<SplittedSpecificatie> bepaalSpecificaties(CervixBetaalopdrachtRegelSpecificatie spec)
	{
		var splittedSpecificaties = new TreeMap<Long, SplittedSpecificatie>();
		for (var boekRegel : spec.getBoekRegels())
		{
			var splittedSpecificatie = splittedSpecificaties.computeIfAbsent(boekRegel.getTarief().getId(), s -> new SplittedSpecificatie());
			var tarief = CervixTariefUtil.getTariefBedrag(boekRegel);
			splittedSpecificatie.tarief = tarief;
			splittedSpecificatie.aantal += (Boolean.FALSE.equals(boekRegel.getDebet()) ? 1 : -1);
			splittedSpecificatie.totaal = splittedSpecificatie.totaal.add(tarief);
		}
		return splittedSpecificaties.values();
	}

	public void insertRegelsEnSpecificaties(List<CervixBetaalopdrachtRegel> regels, String tableNaam, String tableSpecificatieNaam)
	{
		var table = getDataTable(betaalOpdracht, tableNaam);
		for (var regel : regels)
		{
			var naam = "";
			if (regel.getLaboratorium() != null)
			{
				naam = regel.getLaboratorium().getNaam();
			}
			var row = insertRow(table, getNextSequence(), NumberFormat.getCurrencyInstance().format(regel.getBedrag()), naam, regel.getNaarIban());
			for (var spec : regel.getSpecificaties())
			{
				var tableSpecificatie = getDataTable(betaalOpdracht, tableSpecificatieNaam);
				for (var specificatie : bepaalSpecificaties(spec))
				{
					insertRow(tableSpecificatie, getNextSequence(), row.get(tableNaam + ID), spec.getTariefType().getNaam(), regel.getNaarIban(), regel.getNaarTenaamstelling(),
						NumberFormat.getCurrencyInstance().format(specificatie.tarief), specificatie.aantal,
						NumberFormat.getCurrencyInstance().format(specificatie.totaal), regel.getBetaalopdracht().getBetalingskenmerk());
				}
			}
		}
	}

	@Override
	public Document fillExecuteWithRegions(Document document) throws Exception
	{
		log(LOG, betaalOpdracht);
		var mailMerge = document.getMailMerge();
		mailMerge.setCleanupOptions(MailMergeCleanupOptions.REMOVE_UNUSED_REGIONS);
		mailMerge.executeWithRegions(betaalOpdracht);

		return document;
	}
}
