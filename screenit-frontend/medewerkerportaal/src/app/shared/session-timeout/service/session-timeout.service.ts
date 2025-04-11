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
import { Dialog } from '@angular/cdk/dialog'
import { SessionTimeoutComponent } from '@shared/session-timeout/component/session-timeout.component'
import { WINDOW } from '@shared/tokens/window.token'

@Injectable({
  providedIn: 'root',
})
export class SessionTimeoutService {
  tijdGestart = 0
  meldingTimeout = 1500000
  checkTijdInterval: number | undefined
  private dialog: Dialog = inject(Dialog)
  private window: Window = inject(WINDOW)

  resetTimer() {
    this.tijdGestart = new Date().getTime()
  }

  startSessionTimer(): void {
    this.tijdGestart = new Date().getTime()
    this.checkTijdInterval = this.window.setInterval(() => {
      const huidigeTijd: number = new Date().getTime()
      const tijdVerschil: number = huidigeTijd - this.tijdGestart

      if (tijdVerschil > this.meldingTimeout) {
        this.sessieBijnaVerlopen()
        this.window.clearInterval(this.checkTijdInterval)
      }
    }, 1000 * 9)
  }

  sessieBijnaVerlopen(): void {
    this.dialog.open(SessionTimeoutComponent, {
      data: {
        reset: () => this.startSessionTimer(),
        logout: () => this.logout(),
        eindeTimeout: this.tijdGestart + this.meldingTimeout + 300000,
      },
    })
    if (Notification && Notification.permission !== 'denied') {
      const instance = new Notification('ScreenIT - Timeout waarschuwing', {
        body: 'Uw sessie verloopt binnen 5 minuten.',
        icon: '../assets/favicon/favicon-160x160.png',
      })
      instance.onclick = function () {
        this.close()
      }
    }
  }

  logout(): void {
    this.window.screenit.logoutCallback()
  }
}
