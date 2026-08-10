/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { ErrorHandler, inject, Injectable, isDevMode } from '@angular/core'
import { NotificatieService } from '../notificatie/notificatie.service'
import { HttpErrorResponse } from '@angular/common/http'

@Injectable({ providedIn: 'root' })
export class GlobalErrorHandler implements ErrorHandler {
  private readonly notificatieService = inject(NotificatieService)

  handleError(error: Error) {
    if (error.message.startsWith('NG')) {
      return
    }

    let message = 'Er ging iets mis. Neem contact op met de beheerder.'
    if (error instanceof HttpErrorResponse) {
      if (error.error) {
        if (error.error.message) {
          message = error.error.message
          this.showError(message)
        }
      } else {
        this.showError(error.message)
      }
    } else {
      this.showError(message)
    }

    if (isDevMode()) {
      console.error(error)
    }
  }

  private showError(message: string): void {
    this.notificatieService.error(message)
  }
}
