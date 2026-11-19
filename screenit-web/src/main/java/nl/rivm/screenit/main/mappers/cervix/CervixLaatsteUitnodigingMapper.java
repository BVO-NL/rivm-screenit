package nl.rivm.screenit.main.mappers.cervix;

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

import java.time.LocalDateTime;

import nl.rivm.screenit.main.dto.cervix.CervixLaatsteUitnodigingDto;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface CervixLaatsteUitnodigingMapper
{
	@Mapping(target = "rondeNummer", source = "rondeNummer")
	@Mapping(target = "datum", source = "uitnodiging.brief", qualifiedByName = "verstuurdVoorAfdrukkenMoment")
	@Mapping(target = "gebeurtenis", source = "uitnodiging.brief", qualifiedByName = "briefGebeurtenis")
	@Mapping(target = "uitnodigingsId", source = "uitnodiging.uitnodigingsId")
	@Mapping(target = "monsterId", source = "uitnodiging.monster.monsterId")
	@Mapping(target = "wachtOpBrpGegevens", source = "client", qualifiedByName = "wachtOpBrpGegevens")
	CervixLaatsteUitnodigingDto naarDto(CervixUitnodiging uitnodiging, Client client, Integer rondeNummer);

	@Named("verstuurdVoorAfdrukkenMoment")
	default LocalDateTime getVerstuurdVoorAfdrukkenMoment(CervixBrief brief)
	{
		return DateUtil.toLocalDateTime(BriefUtil.getVerstuurdVoorAfdrukkenMoment(brief));
	}

	@Named("briefGebeurtenis")
	default TypeGebeurtenis getBriefGebeurtenis(CervixBrief brief)
	{
		return BriefUtil.isVerstuurdVoorAfdrukken(brief) ? TypeGebeurtenis.BRIEF_AFGEDRUKT : TypeGebeurtenis.BRIEF_KLAARGEZET;
	}

	@Named("wachtOpBrpGegevens")
	default boolean isWachtOpBrpGegevens(Client client)
	{
		return client.getGbaStatus() != GbaStatus.INDICATIE_AANWEZIG;
	}
}
