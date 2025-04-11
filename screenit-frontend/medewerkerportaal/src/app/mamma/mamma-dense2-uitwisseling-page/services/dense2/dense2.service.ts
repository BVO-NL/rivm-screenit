/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Observable } from 'rxjs'
import { MammaDense2Configuratie } from '@shared/types/mamma/mamma-dense2-configuratie'
import { FileService } from '@shared/services/file/file.service'

@Injectable({
  providedIn: 'root',
})
export class Dense2Service {
  private apiService: ApiService = inject(ApiService)
  private fileService: FileService = inject(FileService)
  private baseUrl = '/api/mamma/dense2'
  getConfiguratie(): Observable<MammaDense2Configuratie> {
    return this.apiService.get<MammaDense2Configuratie>(`${this.baseUrl}/configuratie`)
  }

  export(bestandsnaam: string): Observable<Blob> {
    return this.fileService.download(`${this.baseUrl}/export`, bestandsnaam)
  }

  updateConfiguratie(configuratie: MammaDense2Configuratie): Observable<void> {
    return this.apiService.put<void>(`${this.baseUrl}/configuratie`, configuratie)
  }

  import(file: File): Observable<string> {
    const formData = new FormData()
    formData.append('file', file)
    return this.apiService.post<string>(`${this.baseUrl}/import`, formData, { responseType: 'text' })
  }
}
