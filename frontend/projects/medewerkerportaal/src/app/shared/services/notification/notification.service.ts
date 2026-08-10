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
import { DsNotificationConfig, DsNotificationRef, DsNotificationService } from '@topicus-rgp-ds/web'
import { take } from 'rxjs'

interface OpenNotification {
  ref: DsNotificationRef
  config: DsNotificationConfig
}

@Injectable({
  providedIn: 'root',
})
export class NotificationService {
  private dsNotificationService = inject(DsNotificationService)
  private openMessages = new Map<string, OpenNotification>()
  private autoCloseDefault = 5000

  private toonNotificatie(type: 'error' | 'success' | 'warning', message: string, autoClose?: number) {
    if (this.openMessages.has(message)) return

    const config = new DsNotificationConfig()
    config.type = type
    config.message = message
    config.autoClose = autoClose

    const ref = this.dsNotificationService.showNotification(config)
    if (!ref) return

    this.openMessages.set(message, { ref, config })

    ref.notificationRemoved$.pipe(take(1)).subscribe(() => {
      this.openMessages.delete(message)
    })
  }

  error(message: string) {
    this.toonNotificatie('error', message)
  }

  success(message: string) {
    this.toonNotificatie('success', message, this.autoCloseDefault)
  }

  warning(message: string) {
    this.toonNotificatie('warning', message, this.autoCloseDefault)
  }

  hide(message: string) {
    const openNotification = this.openMessages.get(message)
    if (openNotification) {
      this.openMessages.delete(message)
      this.dsNotificationService.removeNotification(openNotification.config)
    }
  }

  clear() {
    this.openMessages.clear()
    this.dsNotificationService.clearAllNotifications()
  }
}
