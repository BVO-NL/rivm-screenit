package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Base64;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import net.fortuna.ical4j.data.CalendarOutputter;
import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.TimeZone;
import net.fortuna.ical4j.model.TimeZoneRegistryFactory;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.Attendee;
import net.fortuna.ical4j.model.property.Created;
import net.fortuna.ical4j.model.property.Description;
import net.fortuna.ical4j.model.property.DtStamp;
import net.fortuna.ical4j.model.property.Location;
import net.fortuna.ical4j.model.property.ProdId;
import net.fortuna.ical4j.model.property.Sequence;
import net.fortuna.ical4j.model.property.Uid;
import net.fortuna.ical4j.model.property.immutable.ImmutableCalScale;
import net.fortuna.ical4j.model.property.immutable.ImmutableMethod;
import net.fortuna.ical4j.model.property.immutable.ImmutableVersion;

import nl.rivm.screenit.service.AfspraakIcalService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

@Slf4j
@AllArgsConstructor
@Service
public class AfspraakIcalServiceImpl implements AfspraakIcalService
{
	private final ICurrentDateSupplier currentDateSupplier;

	private static final String ICALHEADER = "-//BVO NL//ScreenIT//NL";

	@Override
	public Optional<String> maakIcalEventEncoded(LocalDateTime startDatum, LocalDateTime eindDatum, String ontvanger, String locatie, String afspraakId, String omschrijving,
		String content)
	{
		try
		{
			var event = maakEventAan(startDatum, eindDatum, ontvanger, locatie, afspraakId, omschrijving, content);

			var calendar = maakCalendarAan(event);

			var calendarOutputter = new CalendarOutputter();
			var outStream = new ByteArrayOutputStream();

			calendarOutputter.output(calendar, outStream);
			var encodedCalendar = Base64.getEncoder().encodeToString(outStream.toByteArray());

			return Optional.of(encodedCalendar);
		}
		catch (Exception e)
		{
			LOG.error("Er is iets misgegaan met het generen van een ics bestand: {}", e.getMessage());
		}
		return Optional.empty();
	}

	@NotNull
	private VEvent maakEventAan(LocalDateTime startDatum, LocalDateTime eindDatum, String ontvanger, String locatie, String icalAfspraakId, String omschrijving, String content)
	{
		var timeStampStartDatum = createZonedDateTime(startDatum);
		var timeStampEindDatum = createZonedDateTime(eindDatum);
		var timeStampNu = createZonedDateTime(currentDateSupplier.getLocalDateTime());

		var event = new VEvent(timeStampStartDatum, timeStampEindDatum, omschrijving);
		var bestaandeDtStamp = event.getProperty(DtStamp.DTSTAMP);
		bestaandeDtStamp.ifPresent(event::remove);

		event.add(new DtStamp(timeStampNu.toInstant()));
		event.add(new Uid(icalAfspraakId));
		event.add(new Attendee(URI.create(ontvanger)));
		event.add(new Location(locatie));
		event.add(new Description(content));
		event.add(new Sequence(0));
		event.add(new Created(timeStampNu.toInstant()));
		return event;
	}

	@NotNull
	private Calendar maakCalendarAan(VEvent event)
	{
		var calendar = new Calendar();
		calendar.add(new ProdId(ICALHEADER));
		calendar.add(ImmutableVersion.VERSION_2_0);
		calendar.add(ImmutableCalScale.GREGORIAN);
		calendar.add(ImmutableMethod.PUBLISH);
		calendar.add(event);
		calendar.add(getTimezone().getVTimeZone());
		return calendar;
	}

	private TimeZone getTimezone()
	{
		var registry = TimeZoneRegistryFactory.getInstance().createRegistry();
		return registry.getTimeZone(DateUtil.SCREENIT_DEFAULT_ZONE.getId());
	}

	private ZonedDateTime createZonedDateTime(LocalDateTime date)
	{
		return date.atZone(getTimezone().toZoneId());
	}
}
