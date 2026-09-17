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
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'

@Injectable({
  providedIn: 'root',
})
export class MammaClientafspraakService {
  private readonly apiService = inject(ApiService)
  private readonly baseUrl = '/api/mamma/afspraak'

  getAfspraken(clientId: number) {
    return this.apiService.get<MammaAfspraakDto[]>(`${this.baseUrl}?clientId=${clientId}`)
  }

  getAfspraakActies(clientId: number) {
    return this.apiService.get<AfspraakActie[]>(`${this.baseUrl}/acties?clientId=${clientId}`)
  }
}
