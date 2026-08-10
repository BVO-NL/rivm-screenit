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
import { StateService } from '../state/state.service'
import { HttpClient } from '@angular/common/http'
import { map, Observable, take, tap } from 'rxjs'
import { LocatieDto, LocatieStatus } from '../../models/LocatieDto'
import { LocatiesOverzichtDto } from '../../models/LocatiesOverzichtDto'

@Injectable({
  providedIn: 'root',
})
export class LocatieService {
  private readonly stateService = inject(StateService)
  private readonly http = inject(HttpClient)

  getLocaties(pagina: number, aantal: number, status: LocatieStatus): Observable<LocatieDto[]> {
    return this.http
      .post<LocatiesOverzichtDto>('/api/v1/locaties', {
        resultOptions: {
          first: pagina,
          count: aantal,
        },
        status,
      })
      .pipe(
        take(1),
        map((response: LocatiesOverzichtDto) => response.locaties),
        tap((locaties: LocatieDto[]) => this.stateService.setLocaties(locaties)),
      )
  }

  updateLocatie(locatie: LocatieDto): Observable<LocatieDto> {
    return this.http.put<LocatieDto>('/api/v1/locatie', locatie).pipe(
      take(1),
      tap((response: LocatieDto) => this.stateService.updateLocatie(response)),
    )
  }

  verwijderLocatie(locatie: LocatieDto): Observable<LocatieDto> {
    locatie.status = LocatieStatus.INACTIEF
    return this.updateLocatie(locatie)
  }
}
