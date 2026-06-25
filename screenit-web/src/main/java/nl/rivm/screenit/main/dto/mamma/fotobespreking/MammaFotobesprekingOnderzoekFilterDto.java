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
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.main.dto.DurationDto;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;

import io.swagger.v3.oas.annotations.media.Schema;

@Getter
@Setter
@Schema(name = "MammaFotobesprekingOnderzoekFilterDto", description = "Filtercriteria voor het zoeken van mamma-onderzoeken voor fotobespreking.")
public class MammaFotobesprekingOnderzoekFilterDto
{
	private List<MammaOnderzoekRedenFotobespreking> redenFotobesprekingDoorMbber;
	private List<MammaLezingRedenenFotobesprekingMbber> redenFotobesprekingMetMbber;
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenFotobesprekingDoorRadioloog;
	private List<MammaLaesieType> redenDoorverwijzing;

	@Schema(description = "Follow-up status")
	private List<MammaFollowUpConclusieStatus> followUp;

	@Schema(description = "Onderzoeksdatumrange")
	private DurationDto onderzoeksdatum;
	private Long medewerkerId;
	private String bsn;
	private LocalDate geboortedatum;
	private List<Long> beoordelingseenheidIds = new ArrayList<>();
	private List<Long> screeningseenheidIds = new ArrayList<>();
}
