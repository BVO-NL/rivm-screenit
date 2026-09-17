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
import { Pipe, PipeTransform } from '@angular/core'
import { endOfISOWeek, format, getISOWeek, startOfISOWeek } from 'date-fns'
import { nl } from 'date-fns/locale'

@Pipe({
  name: 'datumNaarWeek',
})
export class DatumNaarWeekPipe implements PipeTransform {
  transform(value: Date | string | null): string {
    if (!value) {
      return ''
    }

    const datum = new Date(value)
    if (Number.isNaN(datum.getTime())) {
      return ''
    }

    const beginWeek = startOfISOWeek(datum)
    const eindeWeek = endOfISOWeek(datum)
    return `Week ${getISOWeek(datum)} - ${format(beginWeek, 'd MMMM', { locale: nl })} t/m ${format(eindeWeek, 'd MMMM yyyy', { locale: nl })}`
  }
}
