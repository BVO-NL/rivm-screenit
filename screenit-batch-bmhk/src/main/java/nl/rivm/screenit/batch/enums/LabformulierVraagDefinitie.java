package nl.rivm.screenit.batch.enums;

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

import java.util.List;

import lombok.Getter;

import com.google.common.collect.Range;

@Getter
public enum LabformulierVraagDefinitie
{
	HUISARTS_LOCATIE(Integer.class, PageRangeUtil.TOP_THIRD, PageRangeUtil.LEFT, "AGB"),
	DATUM_UITSTRIJK_DAG(Integer.class, PageRangeUtil.TOP_THIRD, PageRangeUtil.FULL_WIDTH, "Dag"),
	DATUM_UITSTRIJK_MAAND(Integer.class, PageRangeUtil.TOP_THIRD, PageRangeUtil.FULL_WIDTH, "Maand"),
	DATUM_UITSTRIJK_JAAR(Integer.class, PageRangeUtil.TOP_THIRD, PageRangeUtil.FULL_WIDTH, "Jaar"),
	KLACHTEN_GEEN(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.LEFT, "geen"),
	KLACHTEN_CONTACTBLOEDINGEN(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.LEFT, "contactbloedingen"),
	KLACHTEN_ABNORMALE_FLUOR_ZONDER_DUIDELIJKE_OORZAAK(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.FULL_WIDTH,
		"klachten van abnormale fluor zonder duidelijke oorzaak"),
	KLACHTEN_INTERMENSTRUEEL_BLOEDVERLIES(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.FULL_WIDTH, "intermenstrueel bloedverlies"),
	KLACHTEN_POSTMENOPAUZAAL_BLOEDVERLIES(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.FULL_WIDTH, "postmenopauzaal bloedverlies"),
	KLACHTEN_ANDERS_NAMELIJK(Boolean.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.LEFT, "anders, namelijk"),
	KLACHTEN_ANDERS_NAMELIJK_VRIJE_TEKST(String.class, PageRangeUtil.SECOND_QUARTER, PageRangeUtil.LEFT, "anders"),
	MENSTRUATIE_NORMAAL(Boolean.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.LEFT, "normaal"),
	MENSTRUATIE_GEEN(Boolean.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.LEFT, "geen menstruatie"),
	MENSTRUATIE_MENOPAUZE(Boolean.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.FULL_WIDTH, "menopauze"),
	MENSTRUATIE_POSTMENOPAUZE(Boolean.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.FULL_WIDTH, "postmenopauze (>1 jaar geen menstruatie)"),
	DATUM_LAATSTE_MENSTRUATIE_DAG(Integer.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.FULL_WIDTH, "Dag"),
	DATUM_LAATSTE_MENSTRUATIE_MAAND(Integer.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.FULL_WIDTH, "Maand"),
	DATUM_LAATSTE_MENSTRUATIE_JAAR(Integer.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.FULL_WIDTH, "Jaar"),
	ANTICONCEPTIE_GEEN(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.LEFT, "geen"),
	ANTICONCEPTIE_PIL(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.LEFT, "pil (hormonale therapie)"),
	ANTICONCEPTIE_IUD_KOPER(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.FULL_WIDTH, "IUD koper"),
	ANTICONCEPTIE_IUD_MIRENA(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.FULL_WIDTH, "hormoonhoudend spiraal", "IUD mirena"),
	ANTICONCEPTIE_ANDERS(Boolean.class, PageRangeUtil.MIDDLE_THIRD, PageRangeUtil.RIGHT, "anders"),
	GEBRUIK_HORMONEN_GEEN(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.RIGHT, "geen"),
	GEBRUIK_HORMONEN_JA_VANWEGE_BORSTKANKER(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.FULL_WIDTH, "ja, vanwege borstklachten"),
	GEBRUIK_HORMONEN_JA_VANWEGE_OVERGANGSKLACHTEN(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.FULL_WIDTH, "ja, vanwege overgangsklachten"),
	GEBRUIK_HORMONEN_JA_VANWEGE(Boolean.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.LEFT, "ja, vanwege"),
	GEBRUIK_HORMONEN_JA_VANWEGE_VRIJE_TEKST(String.class, PageRangeUtil.THIRD_QUARTER, PageRangeUtil.FULL_WIDTH, "ja, vanwege"),
	ASPECT_CERVIX_NORMAAL(Boolean.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.LEFT, "normaal"),
	ASPECT_CERVIX_NIET_GEZIEN(Boolean.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.LEFT, "niet gezien"),
	ASPECT_CERVIX_ABNORMAAL_OF_VERDACHTE_PORTIO(Boolean.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.FULL_WIDTH, "abnormaal of verdachte portio",
		"abnormaal of verdachte portio. Belangrijk! Licht toe."),
	ASPECT_CERVIX_ABNORMAAL_OF_VERDACHTE_PORTIO_VRIJE_TEKST(String.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.FULL_WIDTH, "abnormaal of verdachte portio",
		"abnormaal of verdachte portio. Belangrijk! Licht toe."),
	OPMERKINGEN(Boolean.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.FULL_WIDTH, "Opmerkingen"),
	OPMERKINGEN_VRIJE_TEKST(String.class, PageRangeUtil.BOTTOM_THIRD, PageRangeUtil.FULL_WIDTH, "Opmerkingen"),
	;

	LabformulierVraagDefinitie(Class<?> antwoordType, Range<Float> verticaleRange, Range<Float> horizontaleRange, String... vraagLabel)
	{
		this.antwoordType = antwoordType;
		this.verticaleRange = verticaleRange;
		this.horizontaleRange = horizontaleRange;
		this.vraagLabel = List.of(vraagLabel);
	}

	private final Class<?> antwoordType;

	private final Range<Float> verticaleRange;

	private final Range<Float> horizontaleRange;

	private final List<String> vraagLabel;

	private static class PageRangeUtil
	{
		public static final Range<Float> TOP_THIRD = Range.closedOpen(0f, 0.33f);

		public static final Range<Float> MIDDLE_THIRD = Range.closedOpen(0.33f, 0.66f);

		public static final Range<Float> BOTTOM_THIRD = Range.closedOpen(0.66f, 1f);

		public static final Range<Float> FIRST_QUARTER = Range.closedOpen(0f, 0.25f);

		public static final Range<Float> SECOND_QUARTER = Range.closedOpen(0.25f, 0.5f);

		public static final Range<Float> THIRD_QUARTER = Range.closedOpen(0.5f, 0.75f);

		public static final Range<Float> FOURTH_QUARTER = Range.closedOpen(0.75f, 1f);

		public static final Range<Float> LEFT = Range.closedOpen(0f, 0.5f);

		public static final Range<Float> RIGHT = Range.closedOpen(0.5f, 1f);

		public static final Range<Float> FULL_WIDTH = Range.closedOpen(0f, 1f);
	}
}
