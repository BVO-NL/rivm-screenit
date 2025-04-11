package nl.rivm.screenit.dto.mamma.afspraken;

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

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

@Setter
@Getter
public class MammaAfspraakReserveringDto implements Serializable
{
	final Long capaciteitBlokId;

	final LocalDateTime vanaf;

	final BigDecimal opkomstkans;

	final MammaDoelgroep doelgroep;

	final boolean eersteOnderzoek;

	final Long tehuisId;

	public MammaAfspraakReserveringDto(Long capaciteitBlokId, LocalDateTime vanaf, BigDecimal opkomstkans, String doelgroep, boolean eersteOnderzoek, Long tehuisId)
	{
		this.capaciteitBlokId = capaciteitBlokId;
		this.vanaf = vanaf;
		this.opkomstkans = opkomstkans;
		this.doelgroep = MammaDoelgroep.valueOf(doelgroep);
		this.eersteOnderzoek = eersteOnderzoek;
		this.tehuisId = tehuisId;
	}
}
