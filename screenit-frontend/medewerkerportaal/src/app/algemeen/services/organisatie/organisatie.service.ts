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
import { OrganisatieDto } from '@/shared/types/algemeen/dto/organisatie.dto'
import { OrganisatieType } from '@/shared/types/algemeen/organisatie-type'
import { inject, Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { HttpClient } from '@angular/common/http'

@Injectable({
  providedIn: 'root',
})
export class OrganisatieService {
  private readonly http = inject(HttpClient)
  private readonly baseUrl = '/api/algemeen/organisatie'

  getCentraleEenheden(): Observable<OrganisatieDto[]> {
    return this.http.get<OrganisatieDto[]>(`${this.baseUrl}/centrale-eenheid`)
  }

  getBeoordelingseenheden(): Observable<OrganisatieDto[]> {
    return this.http.get<OrganisatieDto[]>(`${this.baseUrl}/beoordelingseenheid`)
  }

  zoekBeoordelingseenheden(centraleEenheidIds: number[]): Observable<OrganisatieDto[]> {
    return this.http.post<OrganisatieDto[]>(`${this.baseUrl}/beoordelingseenheid/zoeken`, { organisatieType: OrganisatieType.CENTRALE_EENHEID, organisatieIds: centraleEenheidIds })
  }

  zoekScreeningseenheden(beoordelingseenheidIds: number[]): Observable<OrganisatieDto[]> {
    return this.http.post<OrganisatieDto[]>(`${this.baseUrl}/screeningseenheid/zoeken`, {
      organisatieType: OrganisatieType.BEOORDELINGSEENHEID,
      organisatieIds: beoordelingseenheidIds,
    })
  }
}
