/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { HuisartsDto } from '../../models/HuisartsDto'
import { Observable, take, tap } from 'rxjs'
import { StateService } from '../state/state.service'

@Injectable({
  providedIn: 'root',
})
export class HuisartsService {
  private http = inject(HttpClient)
  private stateService = inject(StateService)
  private baseUrl = '/api/v1/huisarts'

  getHuisarts(): Observable<HuisartsDto> {
    return this.http.get<HuisartsDto>(this.baseUrl).pipe(
      take(1),
      tap((huisarts: HuisartsDto) => this.stateService.setHuisarts(huisarts)),
    )
  }

  controleerHuisarts(huisarts: HuisartsDto): Observable<HuisartsDto> {
    return this.http.put<HuisartsDto>(`${this.baseUrl}/controle`, huisarts)
  }

  updateHuisarts(huisarts: HuisartsDto): Observable<HuisartsDto> {
    return this.http.put<HuisartsDto>(this.baseUrl, huisarts).pipe(tap((updatedHuisarts) => this.stateService.setHuisarts(updatedHuisarts)))
  }
}
