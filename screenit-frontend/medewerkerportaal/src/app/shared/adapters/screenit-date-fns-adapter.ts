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
import { Injectable } from '@angular/core'
import { DateFnsAdapter } from '@angular/material-date-fns-adapter'
import { format as formatDate, parse, isValid, subYears } from 'date-fns'

@Injectable()
export class ScreenitDateFnsAdapter extends DateFnsAdapter {

  override parse(value: unknown, parseFormat: string | string[]): Date | null {
    if (typeof value === 'string') {
      const trimmed = value.trim()
      let kandidaat: Date | null = null

      if (/^\d{8}$/.test(trimmed)) {
        kandidaat = parse(trimmed, 'ddMMyyyy', new Date())
      } else if (/^\d{6}$/.test(trimmed)) {
        kandidaat = parse(trimmed, 'ddMMyy', new Date())
      } else if (/^\d{2}-\d{2}-\d{2}$/.test(trimmed)) {
        kandidaat = parse(trimmed, 'dd-MM-yy', new Date())
      }

      if (kandidaat && isValid(kandidaat)) {
        const cutoffJaar = new Date().getFullYear() + 1
        if (kandidaat.getFullYear() > cutoffJaar) {
          kandidaat = subYears(kandidaat, 100)
        }
        return this.alsLocalDate(kandidaat)
      }
    }
    const superResultaat = super.parse(value, parseFormat)
    return superResultaat && isValid(superResultaat) ? this.alsLocalDate(superResultaat) : null
  }

  override toIso8601(date: Date): string {
    return formatDate(date, 'yyyy-MM-dd')
  }

  private alsLocalDate(date: Date): Date {
    date.toJSON = () => formatDate(date, 'yyyy-MM-dd')
    return date
  }
}
