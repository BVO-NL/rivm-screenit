package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesieIcoon;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

public class LaesieDtoMapper implements Serializable
{

	private static final ObjectMapper objectMapper = new ObjectMapper();

	public List<LaesieDto> lezingToLaesieDtos(MammaLezing lezing)
	{
		return lezing.getLaesies().stream().map(this::mammaLaesieToLaesieDto).sorted(getLaesieDtoComparator()).toList();
	}

	public void koppelNieuweLaesiesAanLezing(List<MammaLaesie> nieuweLaesies, MammaLezing lezing)
	{
		var laesies = lezing.getLaesies();
		laesies.clear();
		laesies.addAll(nieuweLaesies);
		laesies.forEach(l -> l.setLezing(lezing));
	}

	public List<LaesieDto> laesieJsonToLaesieDto(String laesieJson)
	{
		List<LaesieDto> laesieDtos;
		try
		{
			laesieDtos = objectMapper.readValue(laesieJson, new TypeReference<>()
			{
			});
		}
		catch (IOException e)
		{
			throw new IllegalStateException("Cannot convert Json to Dto", e);
		}

		var sortedLaesies = laesieDtos.stream().sorted(getLaesieDtoComparator()).toList();
		voegLaesieVolgordeToeAanLaesies(sortedLaesies);
		return sortedLaesies;
	}

	private void voegLaesieVolgordeToeAanLaesies(List<LaesieDto> sortedLaesieDtos)
	{
		sortedLaesieDtos.forEach(laesieDto ->
		{
			var laesiesUitDezelfdeZijde = sortedLaesieDtos.stream().filter(teFilterenLaesie -> teFilterenLaesie.getWelkeBorst().equals(laesieDto.getWelkeBorst())).toList();
			var zijdePrefix = MammaZijde.RECHTER_BORST.equals(laesieDto.getWelkeBorst()) ? "R" : "L";
			var volgordeNummer = laesiesUitDezelfdeZijde.indexOf(laesieDto) + 1;
			laesieDto.setLaesieVolgorde(zijdePrefix + volgordeNummer);
		});
	}

	public List<MammaLaesie> laesieDtosToMammaLaesies(List<LaesieDto> laesieDtos)
	{
		return laesieDtos.stream().map(this::laesieDtoToMammaLaesie).toList();
	}

	public String laesiesDtosToJson(List<LaesieDto> laesieDtos)
	{
		return toJsonString(sortMammaLaesieDtos(laesieDtos));
	}

	public MammaLaesie laesieDtoToMammaLaesie(LaesieDto laesieDto)
	{
		var laesie = createMammaLaesie(laesieDto);
		laesie.setMammaZijde(laesieDto.getWelkeBorst());
		if (laesieDto.getVerticaleDoorsnede() != null)
		{
			var mammaLaesieIcoonHorizontaal = createMammaLaesieIcoon(laesieDto.getVerticaleDoorsnede());
			laesie.setVerticaleDoorsnedeIcoon(mammaLaesieIcoonHorizontaal);
		}
		if (laesieDto.getHorizontaleDoorsnede() != null)
		{
			var mammaLaesieIcoonVerticaal = createMammaLaesieIcoon(laesieDto.getHorizontaleDoorsnede());
			laesie.setHorizontaleDoorsnedeIcoon(mammaLaesieIcoonVerticaal);
		}
		if (laesieDto.getLaesieGrootteInCm() != null)
		{
			laesie.setLaesieGrootteInCm(laesieDto.getLaesieGrootteInCm());
		}
		laesie.setNummer(laesieDto.getNummer());
		laesie.setLaesieVolgorde(laesieDto.getLaesieVolgorde());
		return laesie;
	}

	private MammaLaesie createMammaLaesie(LaesieDto laesieDto)
	{
		switch (laesieDto.getLaesietype())
		{
		case ARCHITECTUURVERSTORING:
			return new MammaArchitectuurverstoringLaesie();
		case ASYMMETRIE:
			var asymmetrieLaesie = new MammaAsymmetrieLaesie();
			asymmetrieLaesie.setAsymmetrieSpecificatie(laesieDto.getAsymmetrieSpecificatie());
			return asymmetrieLaesie;
		case CALCIFICATIES:
			var calcificatiesLaesie = new MammaCalcificatiesLaesie();
			calcificatiesLaesie.setCalcificatiesDistributie(laesieDto.getCalcificatiesDistributie());
			calcificatiesLaesie.setCalcificatiesVorm(laesieDto.getCalcificatiesVorm());
			return calcificatiesLaesie;
		case MASSA:
			var mammaMassaLaesie = new MammaMassaLaesie();
			mammaMassaLaesie.setMassaBegrenzing(laesieDto.getMassaBegrenzing());
			mammaMassaLaesie.setMassaDensiteit(laesieDto.getMassaDensiteit());
			mammaMassaLaesie.setMassaVorm(laesieDto.getMassaVorm());
			return mammaMassaLaesie;
		default:
			throw new IllegalArgumentException("Onbekend laesie type: " + laesieDto.getLaesietype());
		}
	}

	private MammaLaesieIcoon createMammaLaesieIcoon(MammaLaesieIcoonDto mammaLaesieIcoonDto)
	{
		var mammaLaesieIcoon = new MammaLaesieIcoon();
		var posX = BigDecimal.valueOf(mammaLaesieIcoonDto.getX());
		mammaLaesieIcoon.setPositieX(posX.setScale(3, RoundingMode.HALF_UP));
		var posY = BigDecimal.valueOf(mammaLaesieIcoonDto.getY());
		mammaLaesieIcoon.setPositieY(posY.setScale(3, RoundingMode.HALF_UP));
		return mammaLaesieIcoon;
	}

	private List<LaesieDto> sortMammaLaesieDtos(List<LaesieDto> mammaLaesies)
	{
		return mammaLaesies.stream().sorted(Comparator.comparing(LaesieDto::getNummer)).toList();
	}

	public LaesieDto mammaLaesieToLaesieDto(MammaLaesie mammaLaesie)
	{
		var laesieDto = new LaesieDto();
		laesieDto.setLaesietype(mammaLaesie.getMammaLaesieType());
		laesieDto.setWelkeBorst(mammaLaesie.getMammaZijde());
		if (mammaLaesie.getVerticaleDoorsnedeIcoon() != null)
		{
			var mammaLaesieIcoonDtoVerticaal = createMammaLaesieIcoonDto(mammaLaesie.getVerticaleDoorsnedeIcoon());
			laesieDto.setVerticaleDoorsnede(mammaLaesieIcoonDtoVerticaal);
		}
		if (mammaLaesie.getHorizontaleDoorsnedeIcoon() != null)
		{
			var mammaLaesieIcoonDtoHorizontaal = createMammaLaesieIcoonDto(mammaLaesie.getHorizontaleDoorsnedeIcoon());
			laesieDto.setHorizontaleDoorsnede(mammaLaesieIcoonDtoHorizontaal);
		}
		if (mammaLaesie.getLaesieGrootteInCm() != null)
		{
			laesieDto.setLaesieGrootteInCm(mammaLaesie.getLaesieGrootteInCm());
		}

		setMammaAnnotatieSpecificaties(mammaLaesie, laesieDto);
		laesieDto.setNummer(mammaLaesie.getNummer());
		laesieDto.setLaesieVolgorde(mammaLaesie.getLaesieVolgorde());
		return laesieDto;
	}

	private void setMammaAnnotatieSpecificaties(MammaLaesie mammaLaesie, LaesieDto dto)
	{
		switch (mammaLaesie.getMammaLaesieType())
		{
		case ARCHITECTUURVERSTORING,
			 LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES,
			 LEGACY_MASSA_MET_ARCHITECTUURVERSTORING,
			 LEGACY_CONFORM,
			 LEGACY_GEEN_BIJZONDERHEDEN,
			 LEGACY_MASSA_MET_SPICULAE,
			 LEGACY_PROJECTIE_NAAR_LINKS,
			 LEGACY_PROJECTIE_NAAR_RECHTS,
			 LEGACY_MASSA_MET_CALCIFICATIES,
			 LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES,
			 LEGACY_MARKERING,
			 LEGACY_BENIGNE_KALK:
			break;
		case ASYMMETRIE:
			dto.setAsymmetrieSpecificatie(((MammaAsymmetrieLaesie) mammaLaesie).getAsymmetrieSpecificatie());
			break;
		case CALCIFICATIES:
			dto.setCalcificatiesDistributie(((MammaCalcificatiesLaesie) mammaLaesie).getCalcificatiesDistributie());
			dto.setCalcificatiesVorm(((MammaCalcificatiesLaesie) mammaLaesie).getCalcificatiesVorm());
			break;
		case MASSA:
			dto.setMassaBegrenzing(((MammaMassaLaesie) mammaLaesie).getMassaBegrenzing());
			dto.setMassaDensiteit(((MammaMassaLaesie) mammaLaesie).getMassaDensiteit());
			dto.setMassaVorm(((MammaMassaLaesie) mammaLaesie).getMassaVorm());
			break;
		default:
			throw new IllegalArgumentException("Onbekend laesie type: " + mammaLaesie.getMammaLaesieType());
		}
	}

	private MammaLaesieIcoonDto createMammaLaesieIcoonDto(MammaLaesieIcoon mammaLaesieIcoon)
	{
		var mammaLaesieIcoonDto = new MammaLaesieIcoonDto();
		var x = mammaLaesieIcoon.getPositieX().doubleValue();
		mammaLaesieIcoonDto.setX(x);
		var y = mammaLaesieIcoon.getPositieY().doubleValue();
		mammaLaesieIcoonDto.setY(y);
		return mammaLaesieIcoonDto;
	}

	private String toJsonString(List<LaesieDto> laesieDto)
	{
		try
		{
			return objectMapper.writeValueAsString(laesieDto);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalStateException("Cannot convert Dto to Json", e);
		}
	}

	private Comparator<LaesieDto> getLaesieDtoComparator()
	{
		return Comparator.comparing(LaesieDto::getWelkeBorst).thenComparing(LaesieDto::getLaesietype).thenComparing(LaesieDto::getNummer);
	}
}
