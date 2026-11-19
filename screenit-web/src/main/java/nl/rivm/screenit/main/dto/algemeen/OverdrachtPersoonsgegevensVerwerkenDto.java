package nl.rivm.screenit.main.dto.algemeen;

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

import java.util.List;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.algemeen.OverdrachtType;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class OverdrachtPersoonsgegevensVerwerkenDto
{
	private AanvraagBriefStatus status;

	private OverdrachtType type;

	private List<BriefDto> verstuurdeBrieven = List.of();

	private OverdrachtPersoonsgegevensKeuzesDto keuzes = new OverdrachtPersoonsgegevensKeuzesDto();

	private boolean heeftMammaDossier;

	private boolean heeftCervixDossier;

	private boolean heeftColonDossier;

	private Long ontvangenFormulierId;
}
