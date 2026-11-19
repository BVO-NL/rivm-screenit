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
import { map, Observable } from 'rxjs'
import { ColonFeestdagDto } from '@shared/types/colon/colon-feestdag'
import { ApiService } from '@shared/services/api/api.service'
import { NotPersisted } from '@shared/types/not-presisted'
import { normaliseerNaarDate } from '@shared/utils/date-utils'

@Injectable({
  providedIn: 'root',
})
export class FeestdagenService {
  private apiService: ApiService = inject(ApiService)
  private baseUrl = '/api/colon/feestdag'

  getFeestdagen(): Observable<ColonFeestdagDto[]> {
    return this.apiService.get<ColonFeestdagDto[]>(this.baseUrl).pipe(
      map((feestdagen) =>
        feestdagen.map((feestdag) => ({
          ...feestdag,
          datum: normaliseerNaarDate(feestdag.datum)!,
        })),
      ),
    )
  }

  createFeestdag(feestdag: NotPersisted<ColonFeestdagDto>): Observable<void> {
    return this.apiService.post<void>(this.baseUrl, {
      ...feestdag,
      actief: true,
    })
  }

  updateFeestdag(feestdag: ColonFeestdagDto): Observable<void> {
    return this.apiService.put<void>(`${this.baseUrl}/${feestdag.id}`, feestdag)
  }

  removeFeestdag(feestdag: ColonFeestdagDto): Observable<void> {
    return this.apiService.del(`${this.baseUrl}/${feestdag.id}`)
  }
}
