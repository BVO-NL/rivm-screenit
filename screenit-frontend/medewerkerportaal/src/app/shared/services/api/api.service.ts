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
import { HttpClient } from '@angular/common/http'
import { Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class ApiService {
  private http: HttpClient = inject(HttpClient)
  private defaultOptions = {
    withCredentials: true,
  }

  get<T>(path: string, querystringParams?: Record<string, string>, additionalOptions?: Record<string, string>): Observable<T> {
    let url = `${path}`
    if (querystringParams) {
      url += `?${new URLSearchParams(querystringParams).toString()}`
    }
    return this.http.get<T>(url, this.getOptions(additionalOptions))
  }

  post<T>(path: string, body: unknown, additionalOptions?: Record<string, string>): Observable<T> {
    return this.http.post<T>(path, body, this.getOptions(additionalOptions))
  }

  put<T>(path: string, body: unknown): Observable<T> {
    return this.http.put<T>(path, body, {
      withCredentials: true,
    })
  }

  del<T>(path: string): Observable<T> {
    return this.http.delete<T>(path, {
      withCredentials: true,
    })
  }

  private getOptions(additionalOptions = {}) {
    return {
      ...this.defaultOptions,
      ...additionalOptions,
    }
  }
}
