package nl.rivm.screenit.main.mappers.algemeen;

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
import java.util.Optional;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.dto.algemeen.BriefDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensKeuzesDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensVerwerkenDto;
import nl.rivm.screenit.main.dto.algemeen.OverdrachtPersoonsgegevensDto;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.enums.GebeurtenisBron;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;
import org.mapstruct.ReportingPolicy;

@Mapper(config = ScreenitMapperConfig.class, uses = BriefMapper.class, unmappedTargetPolicy = ReportingPolicy.IGNORE)
public interface OverdrachtPersoonsgegevensMapper
{
	@Mappings({
		@Mapping(target = "status", source = "openstaandVerzoek.status"),
		@Mapping(target = "type", source = "openstaandVerzoek.type"),
		@Mapping(target = "verstuurdeBrieven", source = "openstaandVerzoek", qualifiedByName = "getVerstuurdeBrieven"),
		@Mapping(target = "keuzes", source = "openstaandVerzoek", qualifiedByName = "getKeuzes"),
		@Mapping(target = "heeftMammaDossier", source = "client", qualifiedByName = "heeftMammaDossier"),
		@Mapping(target = "heeftCervixDossier", source = "client", qualifiedByName = "heeftCervixDossier"),
		@Mapping(target = "heeftColonDossier", source = "client", qualifiedByName = "heeftColonDossier"),
		@Mapping(target = "ontvangenFormulierId", source = "openstaandVerzoek.ontvangenAanvraagbrief.id"),
	})
	OverdrachtPersoonsgegevensVerwerkenDto verwerkenGegevensNaarDto(Client client, OverdrachtPersoonsgegevens openstaandVerzoek);

	OverdrachtPersoonsgegevens keuzesNaarOverdrachtPersoonsgegevens(OverdrachtPersoonsgegevensKeuzesDto dto);

	@Mapping(target = "bron", source = "overdrachtPersoonsgegevens", qualifiedByName = "bepaalBron")
	OverdrachtPersoonsgegevensDto overdrachtPersoonsgegevensNaarDto(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens, @Context DossierService dossierService);

	List<OverdrachtPersoonsgegevensDto> overdrachtPersoonsgegevensNaarDtos(List<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevens, @Context DossierService dossierService);

	@Named("bepaalBron")
	default GebeurtenisBron bepaalBron(OverdrachtPersoonsgegevens overdrachtPersoonsgegevens, @Context DossierService dossierService)
	{
		return dossierService.bepaalGebeurtenisBron(overdrachtPersoonsgegevens);
	}

	List<BriefDto> brievenNaarDtos(List<AlgemeneBrief> brieven);

	@Named("getKeuzes")
	default OverdrachtPersoonsgegevensKeuzesDto getKeuzes(OverdrachtPersoonsgegevens overdracht)
	{
		var keuzes = new OverdrachtPersoonsgegevensKeuzesDto();
		if (overdracht != null)
		{
			keuzes.setBkGegevens(Boolean.TRUE.equals(overdracht.getBkGegevens()));
			keuzes.setBkBeelden(Boolean.TRUE.equals(overdracht.getBkBeelden()));
			keuzes.setBmhkGegevens(Boolean.TRUE.equals(overdracht.getBmhkGegevens()));
			keuzes.setDkGegevens(Boolean.TRUE.equals(overdracht.getDkGegevens()));
		}
		return keuzes;
	}

	@Named("getVerstuurdeBrieven")
	default List<BriefDto> getVerstuurdeBrieven(OverdrachtPersoonsgegevens overdracht)
	{
		return brievenNaarDtos(getBrieven(overdracht));
	}

	private List<AlgemeneBrief> getBrieven(OverdrachtPersoonsgegevens overdracht)
	{
		if (overdracht == null)
		{
			return List.of();
		}
		var brieven = new ArrayList<AlgemeneBrief>();
		Optional.ofNullable(overdracht.getVerstuurdeAanvraagbrief()).ifPresent(brieven::add);
		Optional.ofNullable(overdracht.getGeenHandtekeningBrief()).ifPresent(brieven::add);
		brieven.sort(new BriefCreatieDatumComparator());
		return brieven;
	}

	@Named("heeftMammaDossier")
	default boolean heeftMammaDossier(Client client)
	{
		return client.getMammaDossier() != null;
	}

	@Named("heeftCervixDossier")
	default boolean heeftCervixDossier(Client client)
	{
		return client.getCervixDossier() != null;
	}

	@Named("heeftColonDossier")
	default boolean heeftColonDossier(Client client)
	{
		return client.getColonDossier() != null;
	}
}
