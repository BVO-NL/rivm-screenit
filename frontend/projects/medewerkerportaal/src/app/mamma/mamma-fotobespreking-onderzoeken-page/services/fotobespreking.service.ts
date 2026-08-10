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
import { map, Observable } from 'rxjs'
import { MammaFotobesprekingDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking.dto'
import {
  MammaFotobesprekingOnderzoekFilterOpties,
  MammaFotobesprekingOnderzoekFilterOptiesDto,
} from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek-filter-opties'
import { MammaOnderzoekRedenFotobespreking, mammaOnderzoekRedenFotobesprekingLabels } from '@shared/types/mamma/enum/mamma-onderzoek-reden-fotobespreking'
import { EnumOptie } from '@shared/types/enum-optie'
import { MammaLezingRedenenFotobesprekingMbber, mammaLezingRedenenFotobesprekingMbberLabels } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-mbber'
import { MammaLezingRedenenFotobesprekingRadioloog, mammaLezingRedenenFotobesprekingRadioloogLabels } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-radioloog'
import { MammaLaesieType, mammaLaesieTypeLabels } from '@shared/types/mamma/enum/mamma-laesie-type'
import { MammaFollowUpConclusieStatus, mammaFollowUpConclusieStatusLabel } from '@shared/types/mamma/enum/mamma-follow-up-conclusie-status'

@Injectable({
  providedIn: 'root',
})
export class FotobesprekingService {
  private readonly http = inject(HttpClient)
  private readonly baseUrl = '/api/mamma/fotobespreking'

  getFotobespreking(id: number): Observable<MammaFotobesprekingDto> {
    return this.http.get<MammaFotobesprekingDto>(`${this.baseUrl}/${id}`)
  }

  getFilterOpties(): Observable<MammaFotobesprekingOnderzoekFilterOpties> {
    return this.http.get<MammaFotobesprekingOnderzoekFilterOptiesDto>(`${this.baseUrl}/filter-opties`).pipe(
      map((opties) => ({
        redenFotobesprekingDoorMbber: opties.redenFotobesprekingDoorMbber.sort().map(
          (waarde): EnumOptie<MammaOnderzoekRedenFotobespreking> => ({
            waarde,
            naam: mammaOnderzoekRedenFotobesprekingLabels[waarde],
          }),
        ),
        redenFotobesprekingMetMbber: opties.redenFotobesprekingMetMbber.sort().map(
          (waarde): EnumOptie<MammaLezingRedenenFotobesprekingMbber> => ({
            waarde,
            naam: mammaLezingRedenenFotobesprekingMbberLabels[waarde],
          }),
        ),
        redenFotobesprekingDoorRadioloog: opties.redenFotobesprekingDoorRadioloog.sort().map(
          (waarde): EnumOptie<MammaLezingRedenenFotobesprekingRadioloog> => ({
            waarde,
            naam: mammaLezingRedenenFotobesprekingRadioloogLabels[waarde],
          }),
        ),
        redenDoorverwijzing: opties.redenDoorverwijzing.sort().map(
          (waarde): EnumOptie<MammaLaesieType> => ({
            waarde,
            naam: mammaLaesieTypeLabels[waarde],
          }),
        ),
        followUp: opties.followUp.sort().map(
          (waarde): EnumOptie<MammaFollowUpConclusieStatus> => ({
            waarde,
            naam: mammaFollowUpConclusieStatusLabel[waarde],
          }),
        ),
      })),
    )
  }

  voegOnderzoekenToeAanFotobespreking(id: number, clientIds: number[]): Observable<string[]> {
    return this.http.put<{ meldingen: string[] }>(`${this.baseUrl}/${id}/onderzoeken`, { clientIds }).pipe(map(({ meldingen }) => meldingen))
  }
}
