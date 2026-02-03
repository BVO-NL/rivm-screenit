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
import { MammaVisitatieWerklijstFilter } from '@shared/types/mamma/mamma-visitatie-werklijst-filter'
import { map, Observable } from 'rxjs'
import { HttpClient } from '@angular/common/http'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { PagineringDto } from '@shared/types/paginering'
import { MammaVisitatieDto } from '@shared/types/mamma/dto/mamma-visitatie.dto'
import { MammaVisitatieRequestDto } from '@/shared/types/mamma/dto/mamma-visitatie-request.dto'
import { ValidationErrors } from '@angular/forms'
import { MammaVisitatieResponseDto } from '@/shared/types/mamma/dto/mamma-visitatie-response.dto'
import { PagedResponse } from '@/shared/types/paged-response'
import { MammaVisitatieOnderdeel } from '@shared/types/mamma/mamma-visitatie-onderdeel'

@Injectable({
  providedIn: 'root',
})
export class MammaVisitatieService {
  private http = inject(HttpClient)
  private baseUrl = '/api/mamma/visitatie'

  getVisitaties(filter: MammaVisitatieWerklijstFilter, sortering: SorteerParameterDto, paginering: PagineringDto): Observable<PagedResponse<MammaVisitatieDto[]>> {
    return this.http.post<PagedResponse<MammaVisitatieDto[]>>(`${this.baseUrl}/zoeken`, {
      sortering,
      paginering,
      data: filter,
    })
  }

  saveVisitatie(
    visitatie: MammaVisitatieRequestDto,
    bestanden: Map<MammaVisitatieOnderdeel, File>,
    rapportageFile?: File,
    vragenlijstFile?: File,
  ): Observable<MammaVisitatieResponseDto> {
    if (visitatie.id) {
      return this.updateVisitatie(visitatie, bestanden, rapportageFile, vragenlijstFile)
    }

    return this.createVisitatie(visitatie, bestanden, rapportageFile, vragenlijstFile)
  }

  createVisitatie(
    visitatie: MammaVisitatieRequestDto,
    bestanden: Map<MammaVisitatieOnderdeel, File>,
    rapportageFile?: File,
    vragenlijstFile?: File,
  ): Observable<MammaVisitatieResponseDto> {
    const formData = this.getVisitatiePayload(visitatie, bestanden, rapportageFile, vragenlijstFile)
    return this.http.post<MammaVisitatieResponseDto>(this.baseUrl, formData)
  }

  updateVisitatie(
    visitatie: MammaVisitatieRequestDto,
    bestanden: Map<MammaVisitatieOnderdeel, File>,
    rapportageFile?: File,
    vragenlijstFile?: File,
  ): Observable<MammaVisitatieResponseDto> {
    const payload = this.getVisitatiePayload(visitatie, bestanden, rapportageFile, vragenlijstFile)
    return this.http.put<MammaVisitatieResponseDto>(`${this.baseUrl}/${visitatie.id}`, payload)
  }

  private getVisitatiePayload(visitatie: MammaVisitatieRequestDto, bestanden: Map<MammaVisitatieOnderdeel, File>, rapportageFile?: File, vragenlijstFile?: File): FormData {
    const formData = new FormData()
    formData.append('metadata', new Blob([JSON.stringify(visitatie)], { type: 'application/json' }))

    for (const [onderdeel, bestand] of bestanden) {
      formData.append(onderdeel, bestand)
    }
    if (rapportageFile) {
      formData.append('rapportage', rapportageFile)
    }
    if (vragenlijstFile) {
      formData.append('vragenlijst', vragenlijstFile)
    }
    return formData
  }

  verwijderVisitatie(visitatie: MammaVisitatieDto): Observable<void> {
    return this.http.delete<void>(`${this.baseUrl}/${visitatie.id}`)
  }

  getVisitatieByOmschrijving(omschrijving: string, message: string): Observable<ValidationErrors | null> {
    return this.http
      .get<MammaVisitatieDto[]>(`${this.baseUrl}/omschrijving?waarde=${omschrijving}`)
      .pipe(map((response: MammaVisitatieDto[]) => (response.length > 0 ? { uniek: message } : null)))
  }
}
