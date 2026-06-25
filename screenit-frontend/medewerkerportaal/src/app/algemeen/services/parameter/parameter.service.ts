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
import { inject, Injectable } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { BaseService } from '@shared/services/base/base.service'
import { Observable, tap } from 'rxjs'
import { Parameter } from '@shared/types/algemeen/parameter'
import { isBefore, startOfDay } from 'date-fns'
import { parseDate } from '@shared/utils/date-utils'

interface ParameterState {
  parameters: Parameter[]
}

@Injectable({
  providedIn: 'root',
})
export class ParameterService extends BaseService<ParameterState> {
  private http = inject(HttpClient)

  getParameters(): Observable<Parameter[]> {
    return this.http.get<Parameter[]>('/api/parameters').pipe(tap((response) => this.set('parameters', response)))
  }

  isVanafDatumBereikt(naam: string): boolean {
    const parameter = this.select('parameters')()?.find((p) => p.naam === naam)
    if (!parameter?.waarde) {
      return false
    }
    const datum = parseDate(parameter.waarde as string)
    return !isBefore(startOfDay(new Date()), startOfDay(datum))
  }

  isDigitaleIntakeBeschikbaar(): boolean {
    return this.isVanafDatumBereikt('COLON_START_DIGITALE_INTAKE')
  }
}
