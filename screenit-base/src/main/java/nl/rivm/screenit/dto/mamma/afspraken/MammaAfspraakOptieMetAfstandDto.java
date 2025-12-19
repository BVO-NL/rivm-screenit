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

import java.time.LocalDateTime;
import java.util.Objects;

import lombok.Getter;

@Getter
public class MammaAfspraakOptieMetAfstandDto extends MammaBaseAfspraakOptieDto implements Comparable<MammaAfspraakOptieMetAfstandDto>
{
	public static final Double ONBEKENDE_AFSTAND = -1.0;

	private final Double afstand;

	public MammaAfspraakOptieMetAfstandDto(Long capaciteitBlokId, LocalDateTime datumTijd, Long standplaatsPeriodeId, Double afstand)
	{
		super(capaciteitBlokId, datumTijd, standplaatsPeriodeId);
		this.afstand = afstand;
	}

	@Override
	public int compareTo(MammaAfspraakOptieMetAfstandDto afspraakOptieDto)
	{
		int compareTo = this.afstand.compareTo(afspraakOptieDto.afstand);
		if (compareTo == 0)
		{
			compareTo = this.getDatumTijd().compareTo(afspraakOptieDto.getDatumTijd());
		}
		return compareTo;
	}

	@Override
	public boolean equals(Object other)
	{
		if (this == other)
		{
			return true;
		}
		if (other == null || getClass() != other.getClass())
		{
			return false;
		}
		MammaAfspraakOptieMetAfstandDto that = (MammaAfspraakOptieMetAfstandDto) other;
		return Objects.equals(getCapaciteitBlokId(), that.getCapaciteitBlokId()) &&
			Objects.equals(getDatumTijd(), that.getDatumTijd()) &&
			Objects.equals(getStandplaatsPeriodeId(), that.getStandplaatsPeriodeId());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getStandplaatsPeriodeId(), getDatumTijd(), getStandplaatsPeriodeId());
	}

	public boolean isAfstandOnbekend()
	{
		return afstand.equals(ONBEKENDE_AFSTAND);
	}
}
