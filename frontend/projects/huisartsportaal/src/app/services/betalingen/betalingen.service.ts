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
import { BetalingenOverzichtDto } from '../../models/BetalingenOverzichtDto'
import { Observable } from 'rxjs'
import { BetalingenFilterEvent } from '../../models/BetalingenFilterEvent'
import { PagineringDto } from '../../models/PagineringDto'
import { SorteerParameter } from '../../models/SorteerParameter'

@Injectable({
  providedIn: 'root',
})
export class BetalingenService {
  private stateService = inject(StateService)
  private http = inject(HttpClient)

  getBetalingen(paginering: PagineringDto, sortering: SorteerParameter, filter?: BetalingenFilterEvent): Observable<BetalingenOverzichtDto> {
    return this.http.post<BetalingenOverzichtDto>('/api/v1/betaling/all', {
      resultOptions: {
        first: (paginering.paginaNummer - 1) * paginering.paginaGrootte,
        count: paginering.paginaGrootte,
        sortOptions: {
          [sortering.veld]: sortering.richting,
        },
      },
      betalingenZoekObject: filter,
    })
  }
}
