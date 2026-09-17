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
import { CervixCisHistorieDto } from '@shared/types/cervix/cervix-cis-historie.dto'
import { Observable } from 'rxjs'

@Injectable()
export class CervixCisHistorieService {
  private readonly http = inject(HttpClient)
  private readonly baseUrl = '/api/cervix/cis-historie'

  getCisHistorie(clientId: number): Observable<CervixCisHistorieDto> {
    return this.http.get<CervixCisHistorieDto>(`${this.baseUrl}/${clientId}`)
  }

  getRondes(cisHistorie: CervixCisHistorieDto): string[] {
    return Object.keys(cisHistorie.rondes).sort((a, b) => this.vergelijkRondes(a, b))
  }

  private vergelijkRondes(rondeA: string, rondeB: string, ascending = false): number {
    let result = 0
    const rondeGetalA = Number(rondeA)
    const rondeGetalB = Number(rondeB)

    if (!Number.isNaN(rondeGetalA) && !Number.isNaN(rondeGetalB)) {
      if (rondeGetalA > rondeGetalB) {
        result = -1
      } else if (rondeGetalA < rondeGetalB) {
        result = 1
      }
    } else if (rondeA.toLowerCase() === 'memo' && rondeB.toLowerCase() === 'bezwaar') {
      result = -1
    } else if (rondeA.toLowerCase() === 'memo' && rondeB.toLowerCase() !== 'bezwaar') {
      result = 1
    } else if (rondeA.toLowerCase() === 'bezwaar') {
      result = 1
    }

    return result * (ascending ? -1 : 1)
  }
}
