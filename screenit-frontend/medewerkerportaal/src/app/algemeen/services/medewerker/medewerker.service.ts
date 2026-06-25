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
import { Medewerker } from '@shared/types/autorisatie/medewerker'
import { Recht } from '@shared/types/autorisatie/recht'

@Injectable({
  providedIn: 'root',
})
export class MedewerkerService {
  private http = inject(HttpClient)
  private readonly baseUrl = '/api/medewerker'

  getMedewerkers(recht?: Recht) {
    let url = this.baseUrl
    if (recht) {
      url += '?recht=' + recht
    }
    return this.http.get<Medewerker[]>(url)
  }
}
