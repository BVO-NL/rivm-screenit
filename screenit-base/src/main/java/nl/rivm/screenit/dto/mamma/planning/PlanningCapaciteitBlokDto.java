package nl.rivm.screenit.dto.mamma.planning;

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

import java.io.Serial;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

import com.google.common.collect.Range;

import static nl.rivm.screenit.util.DateUtil.toLocalTime;

public class PlanningCapaciteitBlokDto extends PlanningConceptEntiteitDto
{
	@Serial
	private static final long serialVersionUID = 1L;

	public Long id;

	public Date vanaf;

	public Date tot;

	public Integer aantalOnderzoeken;

	public String opmerkingen;

	public MammaCapaciteitBlokType blokType;

	public Long screeningsEenheidId;

	public boolean minderValideAfspraakMogelijk;

	@Getter
	@Setter
	private List<PlanningMindervalideReserveringDto> minderValideReserveringen = new ArrayList<>();

	public Range<LocalTime> getCapaciteitBlokRange()
	{
		return Range.closedOpen(toLocalTime(vanaf), toLocalTime(tot));
	}

	public PlanningCapaciteitBlokDto maakKopie()
	{
		var kopie = new PlanningCapaciteitBlokDto();
		kopie.id = this.id;
		kopie.vanaf = this.vanaf != null ? new Date(this.vanaf.getTime()) : null;
		kopie.tot = this.tot != null ? new Date(this.tot.getTime()) : null;
		kopie.aantalOnderzoeken = this.aantalOnderzoeken;
		kopie.opmerkingen = this.opmerkingen;
		kopie.blokType = this.blokType;
		kopie.screeningsEenheidId = this.screeningsEenheidId;
		kopie.minderValideAfspraakMogelijk = this.minderValideAfspraakMogelijk;
		kopie.getMinderValideReserveringen().addAll(
			this.getMinderValideReserveringen().stream()
				.map(reservering -> new PlanningMindervalideReserveringDto(reservering.getId(), reservering.conceptId,
					reservering.getVanaf()))
				.toList());
		kopie.conceptId = this.conceptId;
		return kopie;
	}

	public void verwerkWijzigingenUit(PlanningCapaciteitBlokDto gewijzigdBlok)
	{
		vanaf = gewijzigdBlok.vanaf;
		tot = gewijzigdBlok.tot;
		aantalOnderzoeken = gewijzigdBlok.aantalOnderzoeken;
		opmerkingen = gewijzigdBlok.opmerkingen;
		blokType = gewijzigdBlok.blokType;
		screeningsEenheidId = gewijzigdBlok.screeningsEenheidId;
		minderValideAfspraakMogelijk = gewijzigdBlok.minderValideAfspraakMogelijk;
		getMinderValideReserveringen().clear();
		getMinderValideReserveringen().addAll(
			gewijzigdBlok.getMinderValideReserveringen().stream()
				.map(reservering -> new PlanningMindervalideReserveringDto(reservering.getId(), reservering.conceptId,
					reservering.getVanaf()))
				.toList());
	}
}
