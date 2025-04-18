package nl.rivm.screenit.service.mamma.impl;

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
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.comparator.MammaLaesieComparator;
import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;
import nl.rivm.screenit.service.mamma.MammaLaesieLocatieService;
import nl.rivm.screenit.service.mamma.be.verslag.MammaLaesieTypeMergeField;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@RequiredArgsConstructor
public class MammaBaseLaesieServiceImpl implements MammaBaseLaesieService
{

	private final MammaLaesieLocatieService laesieLocatieService;

	private List<Map<MammaLaesieTypeMergeField, String>> generateMapVoorLaesieType(MammaLezing verslaglezing, MammaLaesieType type, Class<? extends MammaLaesie> clazz)
	{
		return verslaglezing.getLaesies().stream()
			.filter(laesie -> laesie.getMammaLaesieType() == type)
			.map(clazz::cast)
			.sorted(new MammaLaesieComparator())
			.map(this::getMammaLaesieTypeMergeFieldStringMap)
			.toList();
	}

	private Map<MammaLaesieTypeMergeField, String> getMammaLaesieTypeMergeFieldStringMap(MammaLaesie laesie)
	{
		return switch (laesie.getMammaLaesieType())
		{
			case CALCIFICATIES -> getCalcificatiesMap((MammaCalcificatiesLaesie) laesie);
			case ARCHITECTUURVERSTORING -> getArchitectuurVerstoringMap((MammaArchitectuurverstoringLaesie) laesie);
			case MASSA -> getMassaMap((MammaMassaLaesie) laesie);
			case ASYMMETRIE -> getAsymetrieLaesieMap((MammaAsymmetrieLaesie) laesie);
			default -> throw new IllegalStateException("Onbekende laesie type");
		};
	}

	private void generateLaesieMassaTekst(final StringBuilder laesieTekstBuilder, MammaLezing verslaglezing)
	{
		final List<Map<MammaLaesieTypeMergeField, String>> mergeFieldPerLaesie = generateMapVoorLaesieType(verslaglezing, MammaLaesieType.MASSA, MammaMassaLaesie.class);
		if (mergeFieldPerLaesie.isEmpty())
		{
			return;
		}
		for (Map<MammaLaesieTypeMergeField, String> map : mergeFieldPerLaesie)
		{
			buildBaseString("Laesie massa ", map, laesieTekstBuilder);

			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_MASSA_VORM);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_MASSA_DENSITEIT);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_MASSA_BEGRENZING);
			laesieTekstBuilder.append("\r\n");

		}
		laesieTekstBuilder.append("\n");
	}

	private void generateLaesieCalcificatiesTekst(final StringBuilder laesieTekstBuilder, MammaLezing verslaglezing)
	{
		final List<Map<MammaLaesieTypeMergeField, String>> mergeFieldPerLaesie = generateMapVoorLaesieType(verslaglezing, MammaLaesieType.CALCIFICATIES,
			MammaCalcificatiesLaesie.class);

		if (mergeFieldPerLaesie.isEmpty())
		{
			return;
		}
		for (Map<MammaLaesieTypeMergeField, String> map : mergeFieldPerLaesie)
		{
			buildBaseString("Laesie calcificaties ", map, laesieTekstBuilder);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_CALC_VERD_VORM);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_CALC_DISTRIBUTIE);
			laesieTekstBuilder.append("\r\n");

		}
		laesieTekstBuilder.append("\n");
	}

	private void generateLaesieAsymmetrieTekst(final StringBuilder laesieTekstBuilder, MammaLezing verslaglezing)
	{
		final List<Map<MammaLaesieTypeMergeField, String>> mergeFieldPerLaesie = generateMapVoorLaesieType(verslaglezing, MammaLaesieType.ASYMMETRIE, MammaAsymmetrieLaesie.class);

		if (mergeFieldPerLaesie.isEmpty())
		{
			return;
		}
		for (Map<MammaLaesieTypeMergeField, String> map : mergeFieldPerLaesie)
		{
			buildBaseString("Laesie asymmetrie ", map, laesieTekstBuilder);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE);
			keyValueToString(laesieTekstBuilder, map, MammaLaesieTypeMergeField._BK_LAESIE_ASSYMETRIE_SPEC);
			laesieTekstBuilder.append("\r\n");

		}
		laesieTekstBuilder.append("\n");
	}

	private void generateLaesieArchitectuurVerstoringTekst(final StringBuilder laesieTekstBuilder, MammaLezing verslaglezing)
	{
		final List<Map<MammaLaesieTypeMergeField, String>> mergeFieldPerLaesie = generateMapVoorLaesieType(verslaglezing, MammaLaesieType.ARCHITECTUURVERSTORING,
			MammaArchitectuurverstoringLaesie.class);

		if (mergeFieldPerLaesie.isEmpty())
		{
			return;
		}
		for (Map<MammaLaesieTypeMergeField, String> map : mergeFieldPerLaesie)
		{
			buildBaseString("Laesie architectuur verstoring ", map, laesieTekstBuilder);
			laesieTekstBuilder.append("\r\n");
		}
		laesieTekstBuilder.append("\n");

	}

	private void buildBaseString(String laesieTypeTekst, Map<MammaLaesieTypeMergeField, String> laesiesMap, StringBuilder laesieTekstBuilder)
	{
		keyValueToString(laesieTekstBuilder, laesiesMap, MammaLaesieTypeMergeField._BK_LAESIE_VOLGORDE);
		laesieTekstBuilder.append(laesieTypeTekst);
		laesieTekstBuilder.append(laesiesMap.get(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR));
		laesieTekstBuilder.append(":\n");
		keyValueToString(laesieTekstBuilder, laesiesMap, MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE);
		keyValueToString(laesieTekstBuilder, laesiesMap, MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT);
		keyValueToString(laesieTekstBuilder, laesiesMap, MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE);
	}

	private void keyValueToString(StringBuilder stringBuilder, Map<MammaLaesieTypeMergeField, String> laesiesMap, MammaLaesieTypeMergeField field)
	{
		String value = laesiesMap.get(field);
		if (!Strings.isNullOrEmpty(value))
		{
			stringBuilder.append(field.getNaam());
			stringBuilder.append(": ");
			stringBuilder.append(value);
			stringBuilder.append("\n");
		}
	}

	@Override
	public Map<MammaLaesieTypeMergeField, String> getAsymetrieLaesieMap(MammaAsymmetrieLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = getBaseLaesieMap(laesie);
		getLaesieGrootte(laesieMap, laesie.getLaesieGrootteInCm());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_ASSYMETRIE_SPEC, laesie.getAsymmetrieSpecificatie().getNaam());
		return laesieMap;
	}

	@Override
	public Map<MammaLaesieTypeMergeField, String> getArchitectuurVerstoringMap(MammaArchitectuurverstoringLaesie laesie)
	{
		return getBaseLaesieMap(laesie);
	}

	@Override
	public Map<MammaLaesieTypeMergeField, String> getCalcificatiesMap(MammaCalcificatiesLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = getBaseLaesieMap(laesie);
		getLaesieGrootte(laesieMap, laesie.getLaesieGrootteInCm());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_CALC_VERD_VORM, laesie.getCalcificatiesVorm().getNaam());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_CALC_DISTRIBUTIE, laesie.getCalcificatiesDistributie().getNaam());
		return laesieMap;
	}

	@Override
	public Map<MammaLaesieTypeMergeField, String> getMassaMap(MammaMassaLaesie laesie)
	{
		Map<MammaLaesieTypeMergeField, String> laesieMap = getBaseLaesieMap(laesie);
		getLaesieGrootte(laesieMap, laesie.getLaesieGrootteInCm());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_VORM, laesie.getMassaVorm().getNaam());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_DENSITEIT, laesie.getMassaDensiteit().getNaam());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_MASSA_BEGRENZING, laesie.getMassaBegrenzing().getNaam());
		return laesieMap;
	}

	private Map<MammaLaesieTypeMergeField, String> getBaseLaesieMap(MammaLaesie laesie)
	{
		var laesieMap = new EnumMap<MammaLaesieTypeMergeField, String>(MammaLaesieTypeMergeField.class);
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_VOLGORDE, laesie.getLaesieVolgorde());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_ZIJDE, laesie.getMammaZijde().getNaam());
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_KWADRANT, laesieLocatieService.laesie2kwadrant(laesie));
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_DIEPTE, laesieLocatieService.laesie2diepte(laesie) + " een derde");
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_VOLG_NR,
			isVolgnummerNodig(laesie.getLezing().getLaesies(), laesie) ? String.valueOf(laesie.getNummer()) : "");
		return laesieMap;
	}

	private void getLaesieGrootte(Map<MammaLaesieTypeMergeField, String> laesieMap, BigDecimal laesieGrootte)
	{
		laesieMap.put(MammaLaesieTypeMergeField._BK_LAESIE_GROOTTE, laesieGrootte == null ? "" : "%,.1f".formatted(laesieGrootte) + " cm");
	}

	@Override
	public String getAllLaesieTekstVoorVerslagLezing(MammaLezing verslaglezing)
	{
		final StringBuilder laesieTekstBuilder = new StringBuilder();
		generateLaesieMassaTekst(laesieTekstBuilder, verslaglezing);
		generateLaesieCalcificatiesTekst(laesieTekstBuilder, verslaglezing);
		generateLaesieArchitectuurVerstoringTekst(laesieTekstBuilder, verslaglezing);
		generateLaesieAsymmetrieTekst(laesieTekstBuilder, verslaglezing);

		return laesieTekstBuilder.toString();
	}

	@Override
	public boolean isVolgnummerNodig(List<MammaLaesie> alleLaesies, MammaLaesie laesie)
	{
		return alleLaesies.stream().filter(l -> isZelfdeZijdeEnType(l, laesie)).count() > 1;
	}

	private boolean isZelfdeZijdeEnType(MammaLaesie laesie1, MammaLaesie laesie2)
	{
		return laesie2.getMammaZijde().equals(laesie1.getMammaZijde()) && laesie2.getMammaLaesieType().equals(laesie1.getMammaLaesieType());
	}

}
