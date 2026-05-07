/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
import { format, isValid, parse, parseISO } from 'date-fns'
import { DATE_FORMAT, ISO_DATE_FORMAT, LOCAL_TIME_FORMAT, NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'

export function getDateFormat(date: string): string {
  if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/.test(date)) {
    return ISO_DATE_FORMAT
  }
  if (/^\d{2}-\d{2}-\d{4}$/.test(date)) {
    return NL_DATE_FORMAT
  }
  if (/^\d{4}-\d{2}-\d{2}$/.test(date)) {
    return DATE_FORMAT
  }
  return DATE_FORMAT
}

export function formatDate(date?: Date | null): string {
  return date ? format(date, DATE_FORMAT) : ''
}

export function formatNLDate(date: Date): string {
  return format(date, NL_DATE_FORMAT)
}

export function convertNLDateStringToDateString(date: string): string {
  return format(parse(date, NL_DATE_FORMAT, new Date()), DATE_FORMAT)
}

export function parseDate(date: string): Date {
  const dateFormat = getDateFormat(date)
  return dateFormat === ISO_DATE_FORMAT ? parseISO(date) : parse(date, dateFormat, new Date())
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

export function convertLocalTimeStringToTimeString(localTimeString: string): string {
  if (!localTimeString || !/^\d{2}:\d{2}:\d{2}/.test(localTimeString)) {
    return ''
  }
  return localTimeString.substring(0, 5)
}

export function formatDateAsISO(date: Date) {
  return format(date, "yyyy-MM-dd'T'HH:mm:ss")
}

export function formatDateStringAsTime(dateString: string): string {
  return format(new Date(dateString), TIME_FORMAT)
}

export function isValideTijd(tijd: string): boolean {
  if (!tijd) {
    return false
  }

  const tijdString = tijd.trim()
  if (!/^\d{1,2}:\d{2}$/.test(tijdString)) {
    return false
  }

  const [urenString, minutenString] = tijdString.split(':')
  const uren = Number(urenString)
  const minuten = Number(minutenString)

  return Number.isInteger(uren) && Number.isInteger(minuten) && uren >= 0 && uren <= 23 && minuten >= 0 && minuten <= 59
}

export function formatCompleteTijdString(uren: number, minuten: number) {
  return `${formatTijdString(String(uren))}:${formatTijdString(String(minuten))}`
}

export function formatTijdString(tijdString: string) {
  return tijdString.padStart(2, '0')
}

export function normaliseerTijdInvoer(tijd: string | null | undefined): string {
  if (!tijd) {
    return ''
  }

  const tijdString = tijd.trim()
  if (!tijdString) {
    return ''
  }

  if (isValideTijd(tijdString)) {
    const [urenString, minutenString] = tijdString.split(':')
    return `${formatTijdString(urenString)}:${minutenString}`
  }

  if (!/^\d{3,4}$/.test(tijdString)) {
    return tijdString
  }

  const kandidaat = tijdString.length === 3 ? `0${tijdString.substring(0, 1)}:${tijdString.substring(1)}` : `${tijdString.substring(0, 2)}:${tijdString.substring(2)}`

  return isValideTijd(kandidaat) ? kandidaat : tijdString
}

export function maakTijdOpties(stapMinuten = 5): string[] {
  const tijdOpties: string[] = []

  for (let totaalMinuten = 0; totaalMinuten < 24 * 60; totaalMinuten += stapMinuten) {
    const uren = Math.floor(totaalMinuten / 60)
    const minuten = totaalMinuten % 60
    tijdOpties.push(formatCompleteTijdString(uren, minuten))
  }

  return tijdOpties
}

export function timeToSeconds(time: string) {
  return time.split(':').reduce((acc: number, timePart: string) => 60 * acc + Number(timePart), 0)
}

export function normaliseerNaarDate(datumWaarde: unknown): Date | null {
  if (datumWaarde == null) {
    return null
  }

  if (typeof datumWaarde === 'string') {
    const datumString = datumWaarde.trim()
    if (!datumString) {
      return null
    }

    const geparsteDatum = parseDate(datumString)
    if (isValid(geparsteDatum)) {
      return geparsteDatum
    }
    const nativeDate = new Date(datumString)
    return isValid(nativeDate) ? nativeDate : null
  }

  if (datumWaarde instanceof Date) {
    return isValid(datumWaarde) ? datumWaarde : null
  }

  return null
}
