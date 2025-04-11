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
import { ApiService } from '@shared/services/api/api.service'

@Injectable({
  providedIn: 'root',
})
export class DocumentenService {
  private readonly apiService: ApiService = inject(ApiService)

  getById(id: number): Observable<string> {
    return this.apiService.get<Blob>(`/api/algemeen/documenten/${id}`, undefined, { responseType: 'blob' }).pipe(map((blob: Blob) => URL.createObjectURL(blob)))
  }
}
