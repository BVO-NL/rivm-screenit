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
import { computed, inject, Injectable } from '@angular/core'
import { ClientZoekenFilterDto } from '@shared/types/algemeen/dto/client-zoeken-filter.dto'
import { ClientDto } from '@shared/types/algemeen/dto/client.dto'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { TijdelijkAdresDto } from '@shared/types/algemeen/dto/tijdelijk-adres.dto'
import { BvoStatusDto } from '@shared/types/algemeen/dto/bvo-status.dto'
import { map, Observable, of, switchMap, tap, throwError } from 'rxjs'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { HttpClient, HttpErrorResponse } from '@angular/common/http'
import { BaseService } from '@shared/services/base/base.service'
import { catchError } from 'rxjs/operators'
import { ClientBrpGegevensDto } from '@shared/types/algemeen/dto/clientbrpgegevens.dto'
import { ScreeningRondeGebeurtenisDto } from '@shared/types/algemeen/dto/screening-ronde-gebeurtenis.dto'
import { GbaStatus } from '@shared/types/algemeen/enum/gba-status'

interface ClientState {
  client: ClientDto
}

@Injectable({
  providedIn: 'root',
})
export class ClientService extends BaseService<ClientState> {
  public static readonly CLIENT_STORAGE_KEY = 'screenit.client'

  private readonly http: HttpClient = inject(HttpClient)
  private readonly baseUrl = '/api/client'
  clientId = computed(() => this.select('client')().id)

  constructor() {
    super()
    this.herstelClientUitStorage()
  }

  isClientActief(): boolean {
    const client = this.select('client')()
    return client && client.gbaStatus != GbaStatus.AFGEVOERD
  }

  setClient(client: ClientDto) {
    this.set('client', client)
    sessionStorage.setItem(ClientService.CLIENT_STORAGE_KEY, JSON.stringify(client))
  }

  getClient(clientId: number): Observable<ClientDto> {
    return this.http.get<ClientDto>(`${this.baseUrl}/${clientId}`).pipe(tap((client) => this.setClient(client)))
  }

  zoekClienten(filter: ClientZoekenFilterDto) {
    return this.http.post<ClientDto[]>(`${this.baseUrl}/zoeken`, filter)
  }

  updateTijdelijkAdres(dto: TijdelijkAdresDto): Observable<void> {
    return this.http.put<void>(`${this.baseUrl}/${dto.clientId}/tijdelijk-adres`, dto)
  }

  getTijdelijkAdres(clientId: number): Observable<TijdelijkAdresDto | null> {
    return this.http.get<TijdelijkAdresDto | null>(`${this.baseUrl}/${clientId}/tijdelijk-adres`)
  }

  getActieveBvos(clientId: number) {
    return this.http.get<Bevolkingsonderzoek[]>(`${this.baseUrl}/${clientId}/actieve-bvos`)
  }

  getBvoStatus(clientId: number): Observable<BvoStatusDto[]> {
    return this.http.get<BvoStatusDto[]>(`${this.baseUrl}/${clientId}/bvo-status`)
  }

  private herstelClientUitStorage() {
    try {
      const opgeslagenClient = sessionStorage.getItem(ClientService.CLIENT_STORAGE_KEY)
      if (opgeslagenClient) {
        this.set('client', JSON.parse(opgeslagenClient) as ClientDto)
      }
    } catch {
      sessionStorage.removeItem(ClientService.CLIENT_STORAGE_KEY)
    }
  }

  getClientContactgegevens(id: number): Observable<ClientContactgegevensDto> {
    return this.http.get<ClientContactgegevensDto>(`${this.baseUrl}/${id}/contactgegevens`)
  }

  saveContactgegevens(dto: ClientContactgegevensDto): Observable<void> {
    return this.http.put<void>(`${this.baseUrl}/${dto.clientId}/contactgegevens`, dto)
  }

  getClientBrpGegevens(id: number): Observable<ClientBrpGegevensDto> {
    return this.http.get<ClientBrpGegevensDto>(`${this.baseUrl}/${id}/brp-gegevens`)
  }

  getClientBrpTijdelijkAdres(id: number): Observable<TijdelijkAdresDto | null> {
    return this.http
      .get<TijdelijkAdresDto>(`${this.baseUrl}/${id}/brp-tijdelijk-adres`)
      .pipe(catchError((error: HttpErrorResponse) => (error.status === 404 ? of(null) : throwError(() => error))))
  }

  saveClientBrpTijdelijkAdres(id: number, tijdelijkAdres: TijdelijkAdresDto): Observable<void> {
    return this.http.post<void>(`${this.baseUrl}/${id}/brp-tijdelijk-adres`, tijdelijkAdres).pipe(
      switchMap(() => this.getClient(this.select('client')().id)),
      map(() => void 0),
    )
  }

  deleteClientBrpTijdelijkAdres(id: number): Observable<void> {
    return this.http.delete<void>(`${this.baseUrl}/${id}/brp-tijdelijk-adres`).pipe(
      switchMap(() => this.getClient(this.select('client')().id)),
      map(() => void 0),
    )
  }

  getGebeurtenissen(clientId: number, type: string): Observable<ScreeningRondeGebeurtenisDto[]> {
    return this.http.get<ScreeningRondeGebeurtenisDto[]>(`${this.baseUrl}/${clientId}/gebeurtenissen/${type}`)
  }
}
