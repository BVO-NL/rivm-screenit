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
import { VerrichtingenOverzichtDto } from '../../models/VerrichtingenOverzichtDto'
import { Observable } from 'rxjs'
import { VerrichtingenFilterEvent } from '../../models/VerrichtingenFilterEvent'
import { PagineringDto } from '../../models/PagineringDto'
import { SorteerParameter } from '../../models/SorteerParameter'

@Injectable({
  providedIn: 'root',
})
export class VerrichtingenService {
  private http = inject(HttpClient)

  getVerrichtingen(paginering: PagineringDto, sortering: SorteerParameter, filter?: VerrichtingenFilterEvent): Observable<VerrichtingenOverzichtDto> {
    return this.http.post<VerrichtingenOverzichtDto>('/api/v1/verrichting/all', {
      resultOptions: {
        first: (paginering.paginaNummer - 1) * paginering.paginaGrootte,
        count: paginering.paginaGrootte,
        sortOptions: {
          [sortering.veld]: sortering.richting,
        },
      },
      verrichtingenZoekObject: filter,
    })
  }
}
