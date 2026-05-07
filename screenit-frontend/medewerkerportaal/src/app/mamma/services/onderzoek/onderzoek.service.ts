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
import { MammaFotobesprekingOnderzoekFilterDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek-filter.dto'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { PagineringDto } from '@shared/types/paginering'
import { MammaFotobesprekingOnderzoekDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek.dto'
import { PagedResponse } from '@shared/types/paged-response'
import { formatDate } from '@/shared/utils/date-utils'

@Injectable({
  providedIn: 'root',
})
export class OnderzoekService {
  private readonly baseUrl = '/api/mamma/onderzoek'
  private readonly http = inject(HttpClient)

  zoekOnderzoeken(filter: MammaFotobesprekingOnderzoekFilterDto, sortering: SorteerParameterDto, paginering: PagineringDto) {
    return this.http.post<PagedResponse<MammaFotobesprekingOnderzoekDto[]>>(`${this.baseUrl}/zoeken`, {
      sortering,
      paginering,
      data: {
        ...filter,
        geboortedatum: formatDate(filter.geboortedatum),
      },
    })
  }
}
