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
import { inject, Injectable, OnDestroy } from '@angular/core'
import { Observable, tap } from 'rxjs'
import { BaseService } from '@shared/services/base/base.service'
import { ApiService } from '@shared/services/api/api.service'
import { EnvironmentInfoDto } from '@shared/types/algemeen/dto/environment-info.dto'
import { parseDate } from '@shared/utils/date-utils'

interface DateState {
  dateTimeOffset: number
}

@Injectable({
  providedIn: 'root',
})
export class DateService extends BaseService<DateState> implements OnDestroy {
  private static instance: DateService | null = null

  private readonly apiService = inject(ApiService)
  private readonly dateTimeOffset = this.select('dateTimeOffset')

  constructor() {
    super()
    this.set('dateTimeOffset', 0)
    DateService.instance = this
  }

  ngOnDestroy(): void {
    if (DateService.instance === this) {
      DateService.instance = null
    }
  }

  static nu(): Date {
    return new Date(Date.now() - (DateService.instance?.dateTimeOffset() ?? 0))
  }

  getEnvironmentInfo(): Observable<EnvironmentInfoDto> {
    return this.apiService
      .get<EnvironmentInfoDto>('/api/environment-info')
      .pipe(tap((environmentInfo) => this.set('dateTimeOffset', Date.now() - parseDate(environmentInfo.currentDateTime).getTime())))
  }
}
