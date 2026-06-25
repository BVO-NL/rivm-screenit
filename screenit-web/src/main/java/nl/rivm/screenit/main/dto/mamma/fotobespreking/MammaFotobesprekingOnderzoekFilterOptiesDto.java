package nl.rivm.screenit.main.dto.mamma.fotobespreking;

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

import io.swagger.v3.oas.annotations.media.Schema;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;

@Getter
@Setter
@Schema(name = "MammaFotobesprekingOnderzoekFilterOptiesDto", description = "Beschikbare filteropties voor mamma-onderzoeken voor fotobespreking.")
public class MammaFotobesprekingOnderzoekFilterOptiesDto
{
	@Schema(description = "Beschikbare redenen voor fotobespreking door MBB'er")
	private List<MammaOnderzoekRedenFotobespreking> redenFotobesprekingDoorMbber;

	@Schema(description = "Beschikbare redenen voor fotobespreking met MBB'er")
	private List<MammaLezingRedenenFotobesprekingMbber> redenFotobesprekingMetMbber;

	@Schema(description = "Beschikbare redenen voor fotobespreking door radioloog")
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenFotobesprekingDoorRadioloog;

	@Schema(description = "Beschikbare redenen van doorverwijzing")
	private List<MammaLaesieType> redenDoorverwijzing;

	@Schema(description = "Beschikbare follow-up statussen")
	private List<MammaFollowUpConclusieStatus> followUp;
}
