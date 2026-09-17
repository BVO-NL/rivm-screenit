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
import { connectable, map, Observable, switchMap, tap } from 'rxjs'
import { ClientContactDto } from '@shared/types/algemeen/dto/client-contact.dto'
import { NotPersisted } from '@shared/types/not-presisted'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { BaseService } from '@shared/services/base/base.service'

interface ClientContactState {
  aantalContactenMetMelding: number
  contacten: ClientContactDto[]
}

@Injectable({
  providedIn: 'root',
})
export class ClientContactService extends BaseService<ClientContactState> {
  private readonly http = inject(HttpClient)
  private readonly baseUrl = '/api/client-contact'

  getContacten(clientId: number, type?: ClientContactActieType, moetVoorkomen = true): Observable<ClientContactDto[]> {
    let url = `${this.baseUrl}/${clientId}`
    if (type) {
      url += `?type=${type}&moetVoorkomen=${moetVoorkomen}`
    }
    return this.http.get<ClientContactDto[]>(url).pipe(tap((contacten: ClientContactDto[]) => this.set('contacten', contacten)))
  }

  getAantalContactenMetMelding(clientId: number): Observable<number> {
    const aantal$ = connectable(this.http.get<number>(`${this.baseUrl}/${clientId}/aantal-meldingen`).pipe(tap((aantal: number) => this.set('aantalContactenMetMelding', aantal))))
    aantal$.connect()
    return aantal$
  }

  maakContact(contact: NotPersisted<ClientContactDto>): Observable<ClientContactDto> {
    return this.ververstAantalContactenMetMeldingNa(this.http.post<ClientContactDto>(this.baseUrl, contact), () => contact.clientId)
  }

  private ververstAantalContactenMetMeldingNa<T>(response$: Observable<T>, clientId: (response: T) => number): Observable<T> {
    return response$.pipe(switchMap((response) => this.getAantalContactenMetMelding(clientId(response)).pipe(map(() => response))))
  }

  slaContactOp(contact: ClientContactDto): Observable<ClientContactDto> {
    return this.ververstAantalContactenMetMeldingNa(this.http.put<ClientContactDto>(`${this.baseUrl}/${contact.id}`, contact), () => contact.clientId)
  }

  slaNotitieOp(id: number, notitie: string): Observable<ClientContactDto> {
    return this.ververstAantalContactenMetMeldingNa(this.http.put<ClientContactDto>(`${this.baseUrl}/${id}/notitie`, { notitie }), (response) => response.clientId)
  }

  verwijderNotitie(id: number, clientId: number): Observable<void> {
    return this.ververstAantalContactenMetMeldingNa(this.http.delete<void>(`${this.baseUrl}/${id}`), () => clientId)
  }

  verwijderNotitieUitContact(id: number, clientId: number): Observable<void> {
    return this.ververstAantalContactenMetMeldingNa(this.http.delete<void>(`${this.baseUrl}/${id}/notitie`), () => clientId)
  }
}
