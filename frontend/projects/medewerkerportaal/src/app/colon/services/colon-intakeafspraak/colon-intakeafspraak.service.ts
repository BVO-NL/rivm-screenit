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
import { ApiService } from '@shared/services/api/api.service'
import { VrijSlotZonderKamerFilter } from '@shared/types/colon/dto/vrij-slot-zonder-kamer-filter'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { PagineringDto } from '@shared/types/paginering'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { PagedResponse } from '@shared/types/paged-response'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { map, Observable, switchMap } from 'rxjs'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'

@Injectable({
  providedIn: 'root',
})
export class ColonIntakeafspraakService {
  private readonly apiService = inject(ApiService)
  private readonly clientContactService = inject(ClientContactService)
  private readonly baseUrl = '/api/colon/afspraak'

  getAfspraken(clientId: number) {
    return this.apiService.get<ColonIntakeafspraakDto[]>(`${this.baseUrl}?clientId=${clientId}`)
  }

  getAfspraakActies(clientId: number, afspraakId?: number) {
    const afspraakIdParameter = afspraakId === undefined ? '' : `&afspraakId=${afspraakId}`
    return this.apiService.get<AfspraakActie[]>(`${this.baseUrl}/acties?clientId=${clientId}${afspraakIdParameter}`)
  }

  zoekAfspraakslots(query: VrijSlotZonderKamerFilter, paginering: PagineringDto, sortering: SorteerParameterDto) {
    return this.apiService.post<PagedResponse<VrijSlotZonderKamerDto[]>>(`${this.baseUrl}/zoeken?clientId=${query.clientId}`, {
      sortering,
      paginering,
      data: query,
    })
  }

  verplaatsAfspraak(request: ColonAfspraakMakenRequestDto): Observable<ClientAfspraakDto> {
    return this.apiService
      .put<ClientAfspraakDto>(`${this.baseUrl}/${request.afspraakId}/verplaatsen`, request)
      .pipe(switchMap((response: ClientAfspraakDto) => this.clientContactService.getAantalContactenMetMelding(request.clientId).pipe(map(() => response))))
  }
}
