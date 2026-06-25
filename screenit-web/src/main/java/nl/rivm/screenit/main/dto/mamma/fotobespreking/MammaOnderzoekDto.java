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

import java.time.LocalDate;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;

import io.swagger.v3.oas.annotations.media.Schema;

@Getter
@Setter
@Schema(name = "MammaOnderzoekDto", description = "Weergave van een mamma-onderzoek voor fotobespreking.")
public class MammaOnderzoekDto
{
	private Long clientId;
	private String naam;
	private LocalDate geboortedatum;
	private LocalDate onderzoeksdatum;
	private String bsn;
	private String medewerker;
	private List<MammaLaesieType> redenFotobesprekingDoorMbber;
	private List<MammaLezingRedenenFotobesprekingMbber> redenFotobesprekingMetMbber;
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenFotobesprekingDoorRadioloog;
	private MammaOnderzoekRedenFotobespreking redenDoorverwijzing;

	@Schema(description = "Geeft aan of er sprake is van discrepantie")
	private Boolean discrepantie;

	@Schema(description = "Follow-up status")
	private MammaFollowUpConclusieStatus followUp;
}
