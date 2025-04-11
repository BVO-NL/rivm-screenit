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
import { Project } from '@shared/types/algemeen/project'
import { Observable } from 'rxjs'
import { ProjectType } from '@shared/types/algemeen/project-type'

@Injectable({
  providedIn: 'root',
})
export class ProjectService {
  private apiService: ApiService = inject(ApiService)

  getProjecten(projectType?: ProjectType): Observable<Project[]> {
    let url = `/api/algemeen/project`
    if (projectType) {
      url += `?type=${projectType}`
    }
    return this.apiService.get<Project[]>(url)
  }
}
