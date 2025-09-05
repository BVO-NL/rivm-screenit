package nl.rivm.screenit.mamma.se.proxy.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DateUtil
{
	public static final ZoneId SCREENIT_DEFAULT_ZONE = ZoneId.of("Europe/Amsterdam");

	private static Duration offset = Duration.ZERO;

	private static final DateTimeFormatter DEFAULT_DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

	public static void setOffset(Duration newOffset)
	{
		LOG.info("De tijd is gewijzigd van {} naar {}", getCurrentDateTime(), ZonedDateTime.now(SCREENIT_DEFAULT_ZONE).plus(newOffset));
		DateUtil.offset = newOffset;
	}

	public static LocalDateTime getCurrentDateTime()
	{
		return LocalDateTime.now(SCREENIT_DEFAULT_ZONE).plus(DateUtil.offset);
	}

	public static LocalDate getCurrentDate()
	{
		return getCurrentDateTime().toLocalDate();
	}

	public static boolean isVandaag(LocalDate localDate)
	{
		return getCurrentDateTime().toLocalDate().isEqual(localDate);
	}

	public static LocalDateTime getAfgelopenMiddernacht()
	{
		return getCurrentDateTime().toLocalDate().atStartOfDay();
	}

	public static Duration getOffset()
	{
		return DateUtil.offset;
	}

	public static String format(LocalDateTime localDateTime)
	{
		return DEFAULT_DATE_TIME_FORMATTER.format(localDateTime);
	}

	public static Date toUtilDate(LocalDate localDate)
	{
		return Date.from(localDate.atStartOfDay(SCREENIT_DEFAULT_ZONE).toInstant());
	}

	public static Date toUtilDate(LocalDateTime localDateTime)
	{
		return Date.from(localDateTime.atZone(SCREENIT_DEFAULT_ZONE).toInstant());
	}
}
