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
import { inject, Injectable } from '@angular/core'
import { map, Observable } from 'rxjs'
import { ColonFeestdag, ColonFeestdagDto } from '@shared/types/colon/colon-feestdag'
import { format } from 'date-fns'
import { ApiService } from '@shared/services/api/api.service'

@Injectable({
  providedIn: 'root',
})
export class FeestdagenService {
  private apiService: ApiService = inject(ApiService)
  private baseUrl = '/api/colon/feestdag'

  getFeestdagen(): Observable<ColonFeestdag[]> {
    return this.apiService.get<ColonFeestdagDto[]>(this.baseUrl).pipe(
      map((feestdagen: ColonFeestdagDto[]) =>
        feestdagen.map((feestdag: ColonFeestdagDto) => ({
          ...feestdag,
          datum: new Date(feestdag.datum),
        })),
      ),
    )
  }

  createFeestdag(feestdag: ColonFeestdag): Observable<ColonFeestdagDto> {
    return this.apiService.post<ColonFeestdagDto>(this.baseUrl, {
      ...feestdag,
      actief: true,
      datum: format(feestdag.datum, 'yyyy-MM-dd'),
    })
  }

  updateFeestdag(feestdag: ColonFeestdag): Observable<ColonFeestdagDto> {
    return this.apiService.put<ColonFeestdagDto>(`${this.baseUrl}/${feestdag.id}`, { ...feestdag, datum: format(feestdag.datum, 'yyyy-MM-dd') })
  }

  removeFeestdag(feestdag: ColonFeestdag): Observable<void> {
    return this.apiService.del(`${this.baseUrl}/${feestdag.id}`)
  }
}
