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
import { Handleiding, HandleidingDto, HandleidingUploadResultaatDto } from '@shared/types/algemeen/handleiding'
import { ApiService } from '@shared/services/api/api.service'
import { Observable, tap } from 'rxjs'
import { saveAs } from 'file-saver'
import { inject, Injectable } from '@angular/core'
import { HandleidingUpload } from '@shared/utils/file-utils'

@Injectable({
  providedIn: 'root',
})
export class HandleidingenService {
  private apiService: ApiService = inject(ApiService)
  private baseUrl = '/api/algemeen'

  getHandleidingen(): Observable<Handleiding[]> {
    return this.apiService.get<HandleidingDto[]>(`${this.baseUrl}/handleiding`)
  }

  downloadHandleiding(id: number, bestandsnaam: string): Observable<Blob> {
    return this.apiService.get<Blob>(`${this.baseUrl}/document/${id}`, undefined, { responseType: 'blob' }).pipe(tap((blob) => saveAs(blob, bestandsnaam)))
  }

  uploadHandleidingen(uploadItems: HandleidingUpload[]): Observable<HandleidingUploadResultaatDto[]> {
    const formData = new FormData()
    uploadItems.forEach((uploadItem: HandleidingUpload) => {
      formData.append('bestanden', uploadItem.bestand)
      formData.append('bestandsnamen', uploadItem.bestandsnaam)
    })
    return this.apiService.post<HandleidingUploadResultaatDto[]>(`${this.baseUrl}/handleiding`, formData)
  }

  verwijderHandleiding(id: number): Observable<HandleidingUploadResultaatDto> {
    return this.apiService.del<HandleidingUploadResultaatDto>(`${this.baseUrl}/handleiding/${id}`)
  }

  bewerkHandleiding(id: number, bestand: File | null, bestandsnaam: string): Observable<HandleidingUploadResultaatDto> {
    const formData = new FormData()
    if (bestand) {
      formData.append('bestand', bestand)
    }
    formData.append('bestandsnaam', bestandsnaam)
    return this.apiService.put<HandleidingUploadResultaatDto>(`${this.baseUrl}/handleiding/${id}`, formData)
  }
}
