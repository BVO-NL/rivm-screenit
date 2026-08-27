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
import { Observable } from 'rxjs'
import { OnderzoeksresultatenActieDto } from '@shared/types/algemeen/dto/onderzoeksresultaten-actie.dto'
import { BezwaarType, bezwaarTypeLabels, bezwaarTypeSubtitels } from '@/shared/types/algemeen/enum/bezwaar-type'
import { BezwaarGroupViewWrapper } from '@/shared/types/algemeen/bezwaar-group-view-wrapper'
import { BezwaarViewWrapper } from '@/shared/types/algemeen/bezwaar-view-wrapper'
import { BezwaarMomentDto } from '@/shared/types/algemeen/dto/bezwaar-moment.dto'
import { Bevolkingsonderzoek } from '@/shared/types/bevolkingsonderzoek'
import { BriefType } from '@/shared/types/algemeen/enum/brief-type'
import { getCategorieVanBriefType } from '@/shared/utils/brief-utils'
import { BriefDto } from '@shared/types/algemeen/dto/brief.dto'

@Injectable({
  providedIn: 'root',
})
export class BezwaarService {
  private readonly http = inject(HttpClient)
  private readonly baseUrl = '/api/bezwaar'

  vervangDocument(actieId: number, bestand: File): Observable<{ getekendeBriefId: number }> {
    const formData = new FormData()
    formData.append('id', actieId.toString())
    formData.append('entiteit', 'onderzoeksresultatenactie')
    formData.append('bestand', bestand)
    return this.http.put<OnderzoeksresultatenActieDto>(`${this.baseUrl}/onderzoeksresultaten-actie/vervang-document`, formData)
  }

  vervangBezwaarDocument(bezwaarBriefId: number, bestand: File): Observable<void> {
    const formData = new FormData()
    formData.append('id', bezwaarBriefId.toString())
    formData.append('entiteit', 'bezwaar')
    formData.append('bestand', bestand)
    return this.http.put<void>(`${this.baseUrl}/bezwaar-moment/vervang-document`, formData)
  }

  getBezwaarGroupViewWrappers(moment: BezwaarMomentDto | undefined, verzoekTotBezwaarTeZien: boolean) {
    const groepen: BezwaarGroupViewWrapper[] = []

    if (!moment) {
      return groepen
    }

    for (const bezwaar of moment.bezwaren) {
      if (BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER !== bezwaar.type || verzoekTotBezwaarTeZien) {
        const groep = this.getBezwaarGroupViewWrapperFromList(groepen, bezwaar.bevolkingsonderzoek)
        const wrapper = this.getBezwaarViewWrapper(bezwaar.type, true, bezwaar.bevolkingsonderzoek)
        groep.bezwaren.push(wrapper)

        if (!groepen.includes(groep)) {
          groepen.push(groep)
        }
      }
    }

    return groepen
  }

  private getBezwaarGroupViewWrapperFromList(lijstBezwaarGroupViewWrappers: BezwaarGroupViewWrapper[], onderzoek: Bevolkingsonderzoek | undefined) {
    const wrapperName = onderzoek ?? 'ALGEMEEN'
    const bestaandGroupWrapper = lijstBezwaarGroupViewWrappers.find((groupWrapper) => wrapperName === groupWrapper.key)

    if (bestaandGroupWrapper) {
      return bestaandGroupWrapper
    }

    return this.getGroupWrapper(onderzoek)
  }

  private getGroupWrapper(onderzoek: Bevolkingsonderzoek | undefined) {
    if (!onderzoek) {
      return {
        key: 'ALGEMEEN',
        bezwaren: [],
      }
    }

    return {
      key: onderzoek,
      bevolkingsonderzoek: onderzoek,
      bezwaren: [],
    }
  }

  private getBezwaarViewWrapper(type: BezwaarType, actief: boolean, bevolkingsonderzoek: Bevolkingsonderzoek | undefined): BezwaarViewWrapper {
    return {
      type,
      actief,
      bevolkingsonderzoek,
      naam: bezwaarTypeLabels[type],
      subtitel: bezwaarTypeSubtitels[type],
    }
  }

  verstuurBevestigingsbrievenOnderzoeksresultatenActieNogmaals(actieId: number): Observable<void> {
    return this.http.post<void>(`${this.baseUrl}/onderzoeksresultaten-actie/${actieId}/bevestigingsbrieven-nogmaals-versturen`, null)
  }

  verstuurBevestigingsbrievenBezwaarMomentNogmaals(bezwaarMomentId: number): Observable<BriefDto[]> {
    return this.http.post<BriefDto[]>(`${this.baseUrl}/bezwaar-moment/${bezwaarMomentId}/bevestigingsbrieven-nogmaals-versturen`, null)
  }

  activeerBrief(briefId: number, briefType: BriefType): Observable<void> {
    const briefCategorie = getCategorieVanBriefType(briefType)
    return this.http.post<void>(`${this.baseUrl}/brief/${briefCategorie}/${briefId}/activeren`, null)
  }
}
