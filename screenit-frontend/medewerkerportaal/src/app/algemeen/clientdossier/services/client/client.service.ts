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
import { ClientZoekenFilterDto } from '@shared/types/algemeen/dto/client-zoeken-filter.dto'
import { ClientDto } from '@shared/types/algemeen/dto/client.dto'
import { HttpClient } from '@angular/common/http'
import { BaseService } from '@shared/services/base/base.service'

interface ClientState {
  client: ClientDto
}

@Injectable({
  providedIn: 'root',
})
export class ClientService extends BaseService<ClientState> {
  private readonly http: HttpClient = inject(HttpClient)
  private readonly baseUrl = '/api/client'

  zoekClienten(filter: ClientZoekenFilterDto) {
    return this.http.post<ClientDto[]>(`${this.baseUrl}/zoeken`, filter)
  }
}
