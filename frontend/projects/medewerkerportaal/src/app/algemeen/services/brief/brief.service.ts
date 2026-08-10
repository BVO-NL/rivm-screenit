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
import { BriefActie } from '@shared/types/algemeen/enum/brief-actie'
import { BriefType } from '@shared/types/algemeen/enum/brief-type'
import { getCategorieVanBriefType } from '@shared/utils/brief-utils'

@Injectable({
  providedIn: 'root',
})
export class BriefService {
  private readonly http = inject(HttpClient)

  getBriefActies(briefId: number, briefType: BriefType): Observable<BriefActie[]> {
    const briefCategorie = getCategorieVanBriefType(briefType)
    return this.http.get<BriefActie[]>(`/api/brief/${briefCategorie}/${briefId}/acties`)
  }

  getBriefTemplate(briefId: number, briefType: BriefType): Observable<string> {
    const briefCategorie = getCategorieVanBriefType(briefType)
    return this.http
      .get(`/api/brief/${briefCategorie}/${briefId}/template-inzien`, {
        responseType: 'blob' as 'blob',
      })
      .pipe(map((blob: Blob) => URL.createObjectURL(blob)))
  }

  activeerBrief(briefId: number, briefType: BriefType): Observable<void> {
    const briefCategorie = getCategorieVanBriefType(briefType)
    return this.http.post<void>(`/api/brief/${briefCategorie}/${briefId}/activeren`, {})
  }

  houdBriefTegen(briefId: number, briefType: BriefType) {
    const briefCategorie = getCategorieVanBriefType(briefType)
    return this.http.post(`/api/brief/${briefCategorie}/${briefId}/tegenhouden`, {})
  }
}
