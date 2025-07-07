package nl.rivm.screenit.batch.service.impl;

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

import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie;
import nl.rivm.screenit.batch.service.CervixVerwerkTextractLabformulierService;
import nl.rivm.screenit.batch.util.TextractVerwerkenUtil;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.repository.cervix.CervixHuisartsLocatieRepository;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import software.amazon.awssdk.services.textract.model.AnalyzeDocumentResponse;
import software.amazon.awssdk.services.textract.model.Block;
import software.amazon.awssdk.services.textract.model.BlockType;
import software.amazon.awssdk.services.textract.model.BoundingBox;
import software.amazon.awssdk.services.textract.model.EntityType;
import software.amazon.awssdk.services.textract.model.Relationship;
import software.amazon.awssdk.services.textract.model.RelationshipType;
import software.amazon.awssdk.services.textract.model.SelectionStatus;

import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_LAATSTE_MENSTRUATIE_DAG;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_LAATSTE_MENSTRUATIE_JAAR;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_LAATSTE_MENSTRUATIE_MAAND;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_UITSTRIJK_DAG;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_UITSTRIJK_JAAR;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.DATUM_UITSTRIJK_MAAND;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.HUISARTS_LOCATIE;
import static nl.rivm.screenit.batch.enums.LabformulierVraagDefinitie.OPMERKINGEN_VRIJE_TEKST;

@RequiredArgsConstructor
@Service
@Slf4j
public class CervixVerwerkTextractLabformulierServiceImpl implements CervixVerwerkTextractLabformulierService
{
	private final CervixHuisartsLocatieRepository huisartsLocatieRepository;

	private record Antwoord(List<Block> keywords, List<Block> valueBlocks)
	{
		public Antwoord
		{
			if (keywords == null || keywords.isEmpty() || valueBlocks == null || valueBlocks.isEmpty())
			{
				throw new IllegalArgumentException("keywords en valueBlocks moeten beide gevuld zijn");
			}
		}
	}

	@Transactional
	@Override
	public CervixLabformulier vewerkTextractLabformulierResponse(AnalyzeDocumentResponse textractResponse)
	{
		var antwoorden = extraheerAntwoorden(textractResponse);

		var antwoordenPerVraag = koppelAntwoordenAanVraagDefinities(antwoorden);
		vulHuisartsLocatieAntwoord(textractResponse, antwoordenPerVraag);

		return mapAntwoordenNaarLabformulier(antwoordenPerVraag);
	}

	private List<Antwoord> extraheerAntwoorden(AnalyzeDocumentResponse textractResponse)
	{
		var alleBlokkenInDocument = textractResponse.blocks();
		var alleBlokkenPerId = alleBlokkenInDocument.stream().collect(Collectors.toMap(Block::id, Function.identity()));
		return alleBlokkenInDocument.stream()
			.filter(this::isKeyBlock)
			.map(keyBlock -> antwoordVoorKeyBlock(keyBlock, alleBlokkenPerId))
			.filter(Optional::isPresent)
			.map(Optional::get)
			.toList();
	}

	private boolean isKeyBlock(Block block)
	{
		return BlockType.KEY_VALUE_SET == block.blockType() && block.entityTypes().contains(EntityType.KEY);
	}

	private Optional<Antwoord> antwoordVoorKeyBlock(Block keyBlock, Map<String, Block> alleBlokkenPerId)
	{
		var keywords = new ArrayList<Block>();
		var valueBlocks = new ArrayList<Block>();

		keyBlock.relationships().forEach(relatie ->
		{
			if (isChildRelatie(relatie))
			{
				keywords.addAll(haalBlokkenOpUitRelatie(relatie, alleBlokkenPerId));
			}
			else if (relatie.type() == RelationshipType.VALUE)
			{
				var keyValueSets = haalBlokkenOpUitRelatie(relatie,
					alleBlokkenPerId); 
				valueBlocks.addAll(platgeslagenBlokkenUitKeyValueSets(keyValueSets, alleBlokkenPerId));
			}
		});

		if (!keywords.isEmpty() && !valueBlocks.isEmpty())
		{
			return Optional.of(new Antwoord(keywords, valueBlocks));
		}
		return Optional.empty();
	}

	boolean isChildRelatie(Relationship relatie)
	{
		return RelationshipType.CHILD == relatie.type();
	}

	private List<Block> haalBlokkenOpUitRelatie(Relationship relatie, Map<String, Block> alleBlokkenPerId)
	{
		return relatie.ids().stream().map(alleBlokkenPerId::get).toList();
	}

	private List<Block> platgeslagenBlokkenUitKeyValueSets(List<Block> keyValueSets, Map<String, Block> alleBlokkenPerId)
	{
		return keyValueSets.stream()
			.flatMap(valueBlock -> valueBlock.relationships().stream())
			.filter(this::isChildRelatie)
			.flatMap(relatie -> haalBlokkenOpUitRelatie(relatie, alleBlokkenPerId).stream())
			.toList();
	}

	private EnumMap<LabformulierVraagDefinitie, Antwoord> koppelAntwoordenAanVraagDefinities(List<Antwoord> antwoorden)
	{
		var antwoordenPerVraag = new EnumMap<LabformulierVraagDefinitie, Antwoord>(LabformulierVraagDefinitie.class);

		for (var vraagDefinitie : LabformulierVraagDefinitie.values())
		{
			bestMatchendeAntwoordVoorVraagDefinitie(antwoorden, vraagDefinitie)
				.ifPresent(antwoord -> antwoordenPerVraag.put(vraagDefinitie, antwoord));
		}

		return antwoordenPerVraag;
	}

	private Optional<Antwoord> bestMatchendeAntwoordVoorVraagDefinitie(List<Antwoord> antwoorden, LabformulierVraagDefinitie vraagDefinitie)
	{
		var matchPercentagePerAntwoord = new HashMap<Integer, Antwoord>();
		antwoorden.forEach(antwoord ->
		{
			var keywords = antwoord.keywords;
			var aantalKeywords = keywords.size();
			var aantalKeywordsMatchendMetVraagDefinitie = keywords.stream()
				.filter(keyword -> keywordMatchedMetVraagDefinitie(keyword, vraagDefinitie))
				.count();
			var keywordsMatchPercentage = (int) (((float) aantalKeywordsMatchendMetVraagDefinitie / aantalKeywords) * 100);

			if (keywordsMatchPercentage > 0 && isBetereMatch(keywordsMatchPercentage, aantalKeywords, matchPercentagePerAntwoord))
			{
				matchPercentagePerAntwoord.put(keywordsMatchPercentage, antwoord);
			}
		});

		return matchPercentagePerAntwoord.entrySet().stream().max(Map.Entry.comparingByKey()).map(Map.Entry::getValue);
	}

	private boolean isBetereMatch(int matchPercentage, long aantalKeywords, Map<Integer, Antwoord> matchPercentagePerAntwoord)
	{

		var antwoordMetZelfdeMatchPercentage = matchPercentagePerAntwoord.get(matchPercentage);
		return antwoordMetZelfdeMatchPercentage == null || antwoordMetZelfdeMatchPercentage.keywords.size() < aantalKeywords;
	}

	private void vulHuisartsLocatieAntwoord(AnalyzeDocumentResponse textractResponse, EnumMap<LabformulierVraagDefinitie, Antwoord> antwoordenPerVraag)
	{
		textractResponse.blocks().stream()
			.filter(TextractVerwerkenUtil::isValideHuisartsLocatieBlock)
			.findFirst()
			.ifPresent(block -> antwoordenPerVraag.put(HUISARTS_LOCATIE, new Antwoord(List.of(block), List.of(block))));
	}

	private CervixLabformulier mapAntwoordenNaarLabformulier(EnumMap<LabformulierVraagDefinitie, Antwoord> antwoordenPerVraag)
	{
		var labformulier = new CervixLabformulier();
		antwoordenPerVraag.forEach((vraagDefinitie, antwoord) ->
		{
			switch (vraagDefinitie)
			{
			case DATUM_UITSTRIJK_DAG ->
				labformulier.setDatumUitstrijkje(getDatumAntwoordWaarde(DATUM_UITSTRIJK_DAG, DATUM_UITSTRIJK_MAAND, DATUM_UITSTRIJK_JAAR, antwoordenPerVraag));
			case KLACHTEN_GEEN -> labformulier.setKlachtenGeen((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_CONTACTBLOEDINGEN -> labformulier.setKlachtenContactbloedingen((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_ABNORMALE_FLUOR_ZONDER_DUIDELIJKE_OORZAAK ->
				labformulier.setKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_INTERMENSTRUEEL_BLOEDVERLIES -> labformulier.setKlachtenIntermenstrueelBloedverlies((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_POSTMENOPAUZAAL_BLOEDVERLIES -> labformulier.setKlachtenPostmenopauzaalBloedverlies((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_ANDERS_NAMELIJK -> labformulier.setKlachtenAndersNamelijk((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case KLACHTEN_ANDERS_NAMELIJK_VRIJE_TEKST -> labformulier.setKlachtenAndersNamelijkTekst((String) getAntwoordWaarde(vraagDefinitie, antwoord));
			case MENSTRUATIE_NORMAAL -> labformulier.setMenstruatieNormaal((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case MENSTRUATIE_GEEN -> labformulier.setMenstruatieGeenMenstruatie((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case MENSTRUATIE_MENOPAUZE -> labformulier.setMenstruatieMenopauze((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case MENSTRUATIE_POSTMENOPAUZE -> labformulier.setMenstruatiePostmenopauze((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case DATUM_LAATSTE_MENSTRUATIE_DAG -> labformulier.setDatumLaatsteMenstruatie(
				getDatumAntwoordWaarde(DATUM_LAATSTE_MENSTRUATIE_DAG, DATUM_LAATSTE_MENSTRUATIE_MAAND, DATUM_LAATSTE_MENSTRUATIE_JAAR, antwoordenPerVraag));
			case ANTICONCEPTIE_GEEN -> labformulier.setAnticonceptieGeen((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ANTICONCEPTIE_PIL -> labformulier.setAnticonceptiePil((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ANTICONCEPTIE_IUD_KOPER -> labformulier.setAnticonceptieIudKoper((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ANTICONCEPTIE_IUD_MIRENA -> labformulier.setAnticonceptieIudMirena((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ANTICONCEPTIE_ANDERS -> labformulier.setAnticonceptieAnders((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case GEBRUIK_HORMONEN_JA_VANWEGE_BORSTKANKER -> labformulier.setGebruikHormonenJaVanwegeBorstkanker((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case GEBRUIK_HORMONEN_JA_VANWEGE_OVERGANGSKLACHTEN -> labformulier.setGebruikHormonenJaVanwegeOvergangsklachten((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case GEBRUIK_HORMONEN_JA_VANWEGE -> labformulier.setGebruikHormonenJaVanwege((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case GEBRUIK_HORMONEN_JA_VANWEGE_VRIJE_TEKST -> labformulier.setGebruikHormonenJaVanwegeTekst((String) getAntwoordWaarde(vraagDefinitie, antwoord));
			case GEBRUIK_HORMONEN_GEEN -> labformulier.setGebruikHormonenGeen((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ASPECT_CERVIX_NORMAAL -> labformulier.setAspectCervixNormaal((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ASPECT_CERVIX_NIET_GEZIEN -> labformulier.setAspectCervixNietGezien((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ASPECT_CERVIX_ABNORMAAL_OF_VERDACHTE_PORTIO -> labformulier.setAspectCervixAbnormaalOfVerdachtePortio((Boolean) getAntwoordWaarde(vraagDefinitie, antwoord));
			case ASPECT_CERVIX_ABNORMAAL_OF_VERDACHTE_PORTIO_VRIJE_TEKST ->
				labformulier.setAspectCervixAbnormaalOfVerdachtePortioTekst((String) getAntwoordWaarde(vraagDefinitie, antwoord));
			case OPMERKINGEN -> labformulier.setOpmerkingen(StringUtils.isNotEmpty((String) getAntwoordWaarde(OPMERKINGEN_VRIJE_TEKST,
				antwoordenPerVraag.get(OPMERKINGEN_VRIJE_TEKST))));
			case OPMERKINGEN_VRIJE_TEKST -> labformulier.setOpmerkingenTekst((String) getAntwoordWaarde(vraagDefinitie, antwoord));
			case HUISARTS_LOCATIE -> labformulier.setHuisartsLocatie(getHuisartsLocatieAntwoordWaarde(antwoord));
			case DATUM_UITSTRIJK_MAAND, DATUM_UITSTRIJK_JAAR, DATUM_LAATSTE_MENSTRUATIE_MAAND, DATUM_LAATSTE_MENSTRUATIE_JAAR ->
			{

			}
			}
		});
		return labformulier;
	}

	@SuppressWarnings("DataFlowIssue")
	private @Nullable Date getDatumAntwoordWaarde(LabformulierVraagDefinitie dag, LabformulierVraagDefinitie maand, LabformulierVraagDefinitie jaar,
		EnumMap<LabformulierVraagDefinitie, Antwoord> antwoordenPerVraag)
	{
		try
		{
			var dagVanMaand = (Integer) getAntwoordWaarde(dag, antwoordenPerVraag.get(dag));
			int maandVanJaar = (Integer) getAntwoordWaarde(maand, antwoordenPerVraag.get(maand));
			int jaartal = (Integer) getAntwoordWaarde(jaar, antwoordenPerVraag.get(jaar));
			var datumUitstrijk = LocalDate.of(jaartal, maandVanJaar, dagVanMaand);
			var minimaleInTeVullenDatum = LocalDate.of(1999, 12, 31); 
			if (minimaleInTeVullenDatum.isBefore(datumUitstrijk))
			{
				return DateUtil.toUtilDate(datumUitstrijk);
			}
		}
		catch (NullPointerException | DateTimeException e)
		{
			LOG.debug("Fout bepalen datum uitstrijkje", e);
		}
		return null;
	}

	private Object getAntwoordWaarde(LabformulierVraagDefinitie vraagDefinitie, Antwoord antwoord)
	{
		if (vraagDefinitie.getAntwoordType().equals(Integer.class))
		{
			return getIntegerAntwoordWaarde(antwoord.valueBlocks);
		}
		else if (vraagDefinitie.getAntwoordType().equals(Boolean.class))
		{
			var block = antwoord.valueBlocks.get(0);
			return block.blockType() == BlockType.SELECTION_ELEMENT && block.selectionStatus() == SelectionStatus.SELECTED;
		}
		else if (vraagDefinitie.getAntwoordType().equals(String.class))
		{
			return getStringAntwoordWaarde(antwoord.valueBlocks);
		}
		return null;
	}

	private Integer getIntegerAntwoordWaarde(List<Block> valueBlocks)
	{
		try
		{
			return Integer.parseInt(valueBlocks.stream().map(b -> TextractVerwerkenUtil.vervangLettersMetCijfers(b.text())).collect(Collectors.joining()));
		}
		catch (NumberFormatException e)
		{
			LOG.debug("Fout bij ophalen integer waarde", e);
			return null;
		}
	}

	private String getStringAntwoordWaarde(List<Block> valueBlocks)
	{
		var antwoordTekst = valueBlocks.stream().map(Block::text).filter(Objects::nonNull).collect(Collectors.joining(" "));
		return StringUtils.isNotBlank(antwoordTekst) ? antwoordTekst : null;
	}

	private CervixHuisartsLocatie getHuisartsLocatieAntwoordWaarde(Antwoord antwoord)
	{
		if (antwoord != null)
		{
			var block = antwoord.valueBlocks.get(0);
			var huisartsLocatieRuw = block.text(); 
			var huisartsLocatieId = getHuisartsLocatieIdLong(huisartsLocatieRuw);
			if (huisartsLocatieId != null)
			{
				var huisartsLocatie = huisartsLocatieRepository.findById(huisartsLocatieId);
				if (huisartsLocatie.isPresent())
				{
					return huisartsLocatie.get();
				}
				LOG.error("Huisartslocatie niet gevonden voor id: {}", huisartsLocatieId);
			}
		}
		return null;
	}

	private Long getHuisartsLocatieIdLong(String huisartsLocatieRuw)
	{
		var huisartsLocatieIdString = StringUtils.substringBetween(huisartsLocatieRuw, ":", ",");
		try
		{
			return Long.parseLong(StringUtils.trim(TextractVerwerkenUtil.vervangLettersMetCijfers(huisartsLocatieIdString)));
		}
		catch (NumberFormatException e)
		{
			LOG.error("Fout bij ophalen huisarts locatie id uit tekst: {}", huisartsLocatieRuw, e);
		}

		return null;
	}

	private boolean keywordMatchedMetVraagDefinitie(Block keyword, LabformulierVraagDefinitie vraagDefinitie)
	{
		var keywordStaatInEenMogelijkVraagLabel = vraagDefinitie.getVraagLabel().stream()
			.anyMatch(vraagLabel -> StringUtils.contains(vraagLabel, keyword.text()));
		return keywordStaatInEenMogelijkVraagLabel && boundingBoxValtBinnenVraagDefinitie(keyword.geometry().boundingBox(), vraagDefinitie);
	}

	private boolean boundingBoxValtBinnenVraagDefinitie(BoundingBox box, LabformulierVraagDefinitie vraagDefinitie)
	{
		var hoogteRange = vraagDefinitie.getVerticaleRange();
		var boxHoogstePunt = box.top();
		var boxLaagstePunt = box.top() + box.height();
		var valtInHoogte = hoogteRange.contains(boxHoogstePunt) || hoogteRange.contains(boxLaagstePunt);

		var boxLinksPunt = box.left();
		var boxRechtsPunt = box.left() + box.width();
		var breedteRange = vraagDefinitie.getHorizontaleRange();
		var valtInBreedte = breedteRange.contains(boxLinksPunt) || breedteRange.contains(boxRechtsPunt);

		return valtInHoogte && valtInBreedte;
	}
}
