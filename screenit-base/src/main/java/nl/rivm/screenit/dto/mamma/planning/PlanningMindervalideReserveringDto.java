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

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

import org.apache.commons.lang3.StringUtils;

@Getter
@Setter
public class PlanningMindervalideReserveringDto extends PlanningConceptEntiteitDto
{
	private static final DateTimeFormatter UUR_FORMAT = DateTimeFormatter.ofPattern("HH");

	private static final DateTimeFormatter MINUUT_FORMAT = DateTimeFormatter.ofPattern("mm");

	private Long id;

	private LocalTime vanaf;

	public PlanningMindervalideReserveringDto(Long id, UUID conceptId, LocalTime vanaf)
	{
		this.id = id;
		this.vanaf = vanaf;
		this.conceptId = conceptId;
	}

	@SuppressWarnings("unused")
	public void setVanafUur(String uur)
	{
		vanaf = vanaf.withHour(Integer.parseInt(uur));
	}

	@SuppressWarnings("unused")
	public void setVanafMinuut(String minuut)
	{
		vanaf = vanaf.withMinute(Integer.parseInt(minuut));
	}

	@SuppressWarnings("unused")
	public String getVanafUur()
	{
		return StringUtils.leftPad(vanaf.format(UUR_FORMAT), 2, '0');
	}

	@SuppressWarnings("unused")
	public String getVanafMinuut()
	{
		return StringUtils.leftPad(vanaf.format(MINUUT_FORMAT), 2, '0');
	}

	@Override
	public int hashCode()
	{
		return super.hashCode() + vanaf.hashCode();
	}

	@Override
	public boolean equals(Object obj)
	{
		return super.equals(obj) && this.vanaf.equals(((PlanningMindervalideReserveringDto) obj).getVanaf());
	}
}
