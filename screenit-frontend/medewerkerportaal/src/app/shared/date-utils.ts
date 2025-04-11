/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
import { format, parse } from 'date-fns'
import { DATE_FORMAT, LOCAL_TIME_FORMAT, NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'

export function getDateFormat(date: string): string {
  return /^\d{2}-\d{2}-\d{4}.*/.test(date) ? NL_DATE_FORMAT : DATE_FORMAT
}

export function formatDate(date: Date): string {
  return format(date, DATE_FORMAT)
}

export function formatNLDate(date: Date): string {
  return format(date, NL_DATE_FORMAT)
}

export function convertNLDateStringToDateString(date: string): string {
  return format(parse(date, NL_DATE_FORMAT, new Date()), DATE_FORMAT)
}

export function parseDate(date: string): Date {
  const dateFormat = getDateFormat(date)
  return parse(date, dateFormat, new Date())
}

export function parseDateTime(time: string, date: Date): Date {
  return parse(time, TIME_FORMAT, date)
}

export function formatTimeAsISO(time: string, date: Date = new Date()): string {
  const timeFormat = /^\d{2}:\d{2}:\d{2}.*/.test(time) ? LOCAL_TIME_FORMAT : TIME_FORMAT
  return formatDateAsISO(parse(time, timeFormat, date))
}

export function formatTime(date: Date) {
  return format(date, TIME_FORMAT)
}

export function formatTimeFromDateString(dateString: string) {
  return formatTime(new Date(dateString))
}

export function formatDateAsISO(date: Date) {
  return format(date, "yyyy-MM-dd'T'HH:mm:ss")
}

export function formatDateStringAsTime(dateString: string): string {
  return format(new Date(dateString), TIME_FORMAT)
}

export function timeToSeconds(time: string) {
  return time.split(':').reduce((acc: number, timePart: string) => 60 * acc + Number(timePart), 0)
}
