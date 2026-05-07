package nl.rivm.screenit.main.mappers.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaOnderzoekDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.util.NaamUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface MammaOnderzoekMapper
{
	@Mappings({
		@Mapping(target = "clientId", source = "afspraak.uitnodiging.screeningRonde.dossier.client.id"),
		@Mapping(target = "naam", source = "afspraak.uitnodiging.screeningRonde.dossier.client", qualifiedByName = "clientNaam"),
		@Mapping(target = "geboortedatum", source = "afspraak.uitnodiging.screeningRonde.dossier.client.persoon.geboortedatum"),
		@Mapping(target = "onderzoeksdatum", source = "onderzoek.creatieDatum"),
		@Mapping(target = "bsn", source = "afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"),
		@Mapping(target = "medewerker", source = "mammografie.afgerondDoor.medewerker", qualifiedByName = "medewerkerNaam"),
		@Mapping(target = "redenFotobesprekingDoorMbber", source = "laatsteBeoordeling", qualifiedByName = "redenFotobesprekingDoorMbber"),
		@Mapping(target = "redenFotobesprekingMetMbber", source = "laatsteBeoordeling", qualifiedByName = "redenFotobesprekingMetMbber"),
		@Mapping(target = "redenFotobesprekingDoorRadioloog", source = "laatsteBeoordeling", qualifiedByName = "redenFotobesprekingRadioloog"),
		@Mapping(target = "redenDoorverwijzing", source = "onderzoek.redenFotobespreking"),
		@Mapping(target = "discrepantie", source = "laatsteBeoordeling", qualifiedByName = "heeftDiscrepantie"),
		@Mapping(target = "followUp", source = "afspraak.uitnodiging.screeningRonde.followUpConclusieStatus")
	})
	MammaOnderzoekDto mammaOnderzoekToDto(MammaOnderzoek onderzoek);

	@Named("clientNaam")
	default String conversionClientNaam(Client client)
	{
		return NaamUtil.getAanspreekTussenvoegselEnAchternaam(client);
	}

	@Named("medewerkerNaam")
	default String conversionMedewerkerNaam(Medewerker medewerker)
	{
		return NaamUtil.getNaamMedewerker(medewerker);
	}

	@Named("heeftDiscrepantie")
	default Boolean conversionHeeftDiscrepantie(MammaBeoordeling beoordeling)
	{
		return beoordeling != null ? beoordeling.getDiscrepantieLezing() != null : null;
	}

	@Named("redenFotobesprekingDoorMbber")
	default List<MammaLaesieType> conversionRedenFotobesprekingDoorMbber(MammaBeoordeling beoordeling)
	{
		var laesies = new ArrayList<MammaLaesie>();
		if (beoordeling == null)
		{
			return List.of();
		}

		if (beoordeling.getEersteLezing() != null)
		{
			laesies.addAll(beoordeling.getEersteLezing().getLaesies());
		}
		if (beoordeling.getTweedeLezing() != null)
		{
			laesies.addAll(beoordeling.getTweedeLezing().getLaesies());
		}
		if (beoordeling.getDiscrepantieLezing() != null)
		{
			laesies.addAll(beoordeling.getDiscrepantieLezing().getLaesies());
		}
		if (beoordeling.getArbitrageLezing() != null)
		{
			laesies.addAll(beoordeling.getArbitrageLezing().getLaesies());
		}
		return laesies.stream().map(MammaLaesie::getMammaLaesieType).distinct().toList();
	}

	@Named("redenFotobesprekingMetMbber")
	default List<MammaLezingRedenenFotobesprekingMbber> conversionRedenFotobesprekingMetMbber(MammaBeoordeling beoordeling)
	{
		var redenen = new ArrayList<MammaLezingRedenenFotobesprekingMbber>();
		if (beoordeling == null)
		{
			return List.of();
		}

		if (beoordeling.getEersteLezing() != null)
		{
			redenen.addAll(beoordeling.getEersteLezing().getRedenenFotobesprekingMbber());
		}
		if (beoordeling.getTweedeLezing() != null)
		{
			redenen.addAll(beoordeling.getTweedeLezing().getRedenenFotobesprekingMbber());
		}
		if (beoordeling.getDiscrepantieLezing() != null)
		{
			redenen.addAll(beoordeling.getDiscrepantieLezing().getRedenenFotobesprekingMbber());
		}
		if (beoordeling.getArbitrageLezing() != null)
		{
			redenen.addAll(beoordeling.getArbitrageLezing().getRedenenFotobesprekingMbber());
		}
		return redenen.stream().distinct().toList();
	}

	@Named("redenFotobesprekingRadioloog")
	default List<MammaLezingRedenenFotobesprekingRadioloog> conversionRedenFotobesprekingRadioloog(MammaBeoordeling beoordeling)
	{
		var redenen = new ArrayList<MammaLezingRedenenFotobesprekingRadioloog>();
		if (beoordeling == null)
		{
			return List.of();
		}

		if (beoordeling.getEersteLezing() != null)
		{
			redenen.addAll(beoordeling.getEersteLezing().getRedenenFotobesprekingRadioloog());
		}
		if (beoordeling.getTweedeLezing() != null)
		{
			redenen.addAll(beoordeling.getTweedeLezing().getRedenenFotobesprekingRadioloog());
		}
		if (beoordeling.getDiscrepantieLezing() != null)
		{
			redenen.addAll(beoordeling.getDiscrepantieLezing().getRedenenFotobesprekingRadioloog());
		}
		if (beoordeling.getArbitrageLezing() != null)
		{
			redenen.addAll(beoordeling.getArbitrageLezing().getRedenenFotobesprekingRadioloog());
		}
		return redenen.stream().distinct().toList();
	}
}
