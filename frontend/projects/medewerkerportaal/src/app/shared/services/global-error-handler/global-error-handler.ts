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
import { ErrorHandler, inject, Injectable, isDevMode } from '@angular/core'
import { HttpErrorResponse } from '@angular/common/http'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ALGEMEEN_STRINGS as strings } from '@shared/strings/algemeen.strings'

const NEGEER_MELDING_PREFIXES = [
  'NG',
  'ResizeObserver loop',
]

@Injectable({ providedIn: 'root' })
export class GlobalErrorHandler implements ErrorHandler {
  private notificationService = inject(NotificationService)

  handleError(error: Error) {
    if (isDevMode()) {
      console.error(error)
    }

    if (this.moetGenegeerdWorden(error)) {
      return
    }

    let message = strings.errors.exceptie
    if (error instanceof HttpErrorResponse) {
      if (error.error) {
        if (error.error.message) {
          message = error.error.message
          if (error.error.additionalInfo) {
            message = `${message} ${error.error.additionalInfo}`
          }
          this.showError(message)
        } else if (error.error['messages']) {
          error.error['messages'].forEach((message: string) => this.showError(message))
        } else {
          this.showError(message)
        }
      } else {
        this.showError(message)
      }
    } else {
      this.showError(error.message)
    }
  }

  private moetGenegeerdWorden(error: Error): boolean {
    const melding = error.cause instanceof ErrorEvent ? error.cause.message : error.message
    return NEGEER_MELDING_PREFIXES.some((prefix) => melding.startsWith(prefix))
  }

  private showError(message: string): void {
    this.notificationService.error(message)
  }
}
