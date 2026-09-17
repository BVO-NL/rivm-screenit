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
import { inject, Injectable, Signal } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { ColonIntakelocatie } from '@shared/types/colon/colon-intakelocatie'
import { BaseService } from '@shared/services/base/base.service'
import { Observable, take, tap } from 'rxjs'

interface ColonState {
  intakelocatie: ColonIntakelocatie
}

@Injectable({
  providedIn: 'root',
})
export class ColonIntakelocatieService extends BaseService<ColonState> {
  private readonly http: HttpClient = inject(HttpClient)
  private readonly baseUrl = '/api/colon/intakelocatie'

  fetchIntakelocatie(): Observable<ColonIntakelocatie> {
    return this.http.get<ColonIntakelocatie>(this.baseUrl).pipe(
      take(1),
      tap((intakelocatie) => this.set('intakelocatie', intakelocatie)),
    )
  }

  getIntakelocatieById(id: number): Observable<ColonIntakelocatie> {
    return this.http.get<ColonIntakelocatie>(`${this.baseUrl}/${id}`)
  }

  get intakelocatie(): Signal<ColonIntakelocatie> {
    return this.select('intakelocatie')
  }
}
