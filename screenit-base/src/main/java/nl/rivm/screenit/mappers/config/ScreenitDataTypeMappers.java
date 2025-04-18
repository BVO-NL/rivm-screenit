package nl.rivm.screenit.mappers.config;

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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
public class ScreenitDataTypeMappers
{
	public String convertBlankStringToNull(String input)
	{
		return StringUtils.trimToNull(input);
	}

	public LocalDateTime convertDateToLocalDateTime(Date date)
	{
		return DateUtil.toLocalDateTime(date);
	}

	public Date convertLocalDateTimeToDate(LocalDateTime localDateTime)
	{
		return DateUtil.toUtilDate(localDateTime);
	}

	public LocalDate convertDateToLocalDate(Date date)
	{
		return DateUtil.toLocalDate(date);
	}

	public Date convertLocalDateToDate(LocalDate localDate)
	{
		return DateUtil.toUtilDate(localDate);
	}

}
