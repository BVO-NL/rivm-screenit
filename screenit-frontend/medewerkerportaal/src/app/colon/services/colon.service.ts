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
import { inject, Injectable, Signal } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { Intakelocatie } from '@shared/types/intakelocatie'
import { BaseService } from '@shared/services/base/base.service'
import { Observable, take, tap } from 'rxjs'

interface ColonState {
  intakelocatie: Intakelocatie
}

@Injectable({
  providedIn: 'root',
})
export class ColonService extends BaseService<ColonState> {
  private http: HttpClient = inject(HttpClient)

  fetchIntakelocatie(): Observable<Intakelocatie> {
    return this.http.get<Intakelocatie>('/api/colon/intakelocatie').pipe(
      take(1),
      tap((intakelocatie) => this.set('intakelocatie', intakelocatie)),
    )
  }

  get intakelocatie(): Signal<Intakelocatie> {
    return this.select('intakelocatie')
  }
}
