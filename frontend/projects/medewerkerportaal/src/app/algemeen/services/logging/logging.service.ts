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
import { ApiService } from '@shared/services/api/api.service'
import { connectable, Observable } from 'rxjs'
import { LoggingDto } from '@shared/types/algemeen/dto/logging.dto'
import { LogGebeurtenis } from '@shared/types/algemeen/enum/log-gebeurtenis'

@Injectable({
  providedIn: 'root',
})
export class LoggingService {
  private readonly apiService: ApiService = inject(ApiService)

  logGebeurtenis(logGebeurtenis: LogGebeurtenis, omschrijving: string | null = null, clientId?: number): Observable<void> {
    const loggingDto: LoggingDto = { logGebeurtenis, omschrijving, clientId }
    const response$ = connectable(this.apiService.post<void>('/api/logging', loggingDto))
    response$.connect()
    return response$
  }
}
