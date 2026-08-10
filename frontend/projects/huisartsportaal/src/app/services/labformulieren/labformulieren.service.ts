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
import { LabformulierAanvragenOverzichtDto } from '../../models/LabformulierAanvragenOverzichtDto'
import { Observable, take, tap } from 'rxjs'
import { PagineringDto } from '../../models/PagineringDto'
import { SorteerParameter } from '../../models/SorteerParameter'

@Injectable({
  providedIn: 'root',
})
export class LabformulierenService {
  private stateService = inject(StateService)
  private http = inject(HttpClient)

  getFormulieren(paginering: PagineringDto, sortering: SorteerParameter): Observable<LabformulierAanvragenOverzichtDto> {
    return this.http
      .post<LabformulierAanvragenOverzichtDto>('/api/v1/aanvragen/huisarts', {
        resultOptions: {
          first: (paginering.paginaNummer - 1) * paginering.paginaGrootte,
          count: paginering.paginaGrootte,
          sortOptions: {
            [sortering.veld]: sortering.richting,
          },
        },
      })
      .pipe(
        take(1),
        tap((response: LabformulierAanvragenOverzichtDto) => {
          this.stateService.setLabformulieren(response.aanvragen)
        }),
      )
  }
}
