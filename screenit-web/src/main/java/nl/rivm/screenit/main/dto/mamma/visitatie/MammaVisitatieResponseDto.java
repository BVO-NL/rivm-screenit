package nl.rivm.screenit.main.dto.mamma.visitatie;

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

import io.swagger.v3.oas.annotations.media.Schema;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Schema(name = "MammaVisitatieResponseDto", description = "Resultaat van het aanmaken of bijwerken van een mamma-visitatie.")
public class MammaVisitatieResponseDto
{
	@Schema(description = "Meldingen die tijdens de verwerking zijn ontstaan")
	private List<String> meldingen = new ArrayList<>();

	@Schema(description = "De verwerkte visitatie")
	private MammaVisitatieDto visitatie;
}
