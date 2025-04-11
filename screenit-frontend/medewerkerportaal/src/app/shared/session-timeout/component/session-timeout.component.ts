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
import { Component, inject, OnInit } from '@angular/core'
import { DOCUMENT } from '@angular/common'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { WINDOW } from '@shared/tokens/window.token'

declare global {
  interface Window {
    screenit: {
      keepAliveCallback: () => void
      logoutCallback: () => void
    }
  }
}

@Component({
  selector: 'app-session-timeout',
  imports: [BaseDialogComponent],
  templateUrl: './session-timeout.component.html',
  styleUrls: ['./session-timeout.component.scss'],
})
export class SessionTimeoutComponent implements OnInit {
  timer = ''
  interval: number | undefined
  dialogData: { reset: () => void; logout: () => void; eindeTimeout: number } = inject(DIALOG_DATA)
  dialogRef = inject(DialogRef)
  document: Document = inject(DOCUMENT)
  window: Window = inject(WINDOW)

  ngOnInit() {
    let minuten = 5
    let seconden = 0
    this.interval = this.window.setInterval(() => {
      const tijdOver = this.berekenTijdOver()
      minuten = Math.floor(tijdOver / 60000)
      seconden = Math.floor((tijdOver - minuten * 60000) / 1000)
      if (minuten < 0) {
        this.dialogData.logout()
        return
      }
      let minutenTekst = ''
      if (minuten > 0) {
        minutenTekst = `${minuten}${minuten > 1 ? ' minuten ' : ' minuut '}`
      }
      const secondenTekst = seconden > 1 ? 'seconden' : 'seconde'

      this.timer = `${minutenTekst}${seconden} ${secondenTekst}`
      this.document.title = `ScreenIT - ${minuten}m${seconden}s (Uw sessie verloopt)`
    }, 1000)
  }

  berekenTijdOver() {
    return this.dialogData.eindeTimeout - new Date().getTime()
  }

  keepAlive() {
    this.window.screenit.keepAliveCallback()
    this.window.clearInterval(this.interval)
    this.document.title = 'ScreenIT'
    this.dialogData.reset()
    this.dialogRef.close()
  }
}
