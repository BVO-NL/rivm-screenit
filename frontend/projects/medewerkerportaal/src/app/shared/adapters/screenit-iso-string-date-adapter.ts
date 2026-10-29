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
import { DsIsostringDateAdapter, provideDsDateFormats } from '@topicus-rgp-ds/web'
import { DateService } from '../services/date/date.service'
import { DateAdapter } from '@angular/material/core'
import { formatDateAsISO } from '@shared/utils/date-utils'

@Injectable({ providedIn: 'root' })
export class ScreenitIsoStringDateAdapter extends DsIsostringDateAdapter {
  override today(): string {
    return formatDateAsISO(DateService.nu())
  }
}

export function provideScreenitIsoStringDateAdapter() {
  return [
    provideDsDateFormats(),
    {
      provide: DateAdapter,
      useClass: ScreenitIsoStringDateAdapter,
    },
  ]
}
