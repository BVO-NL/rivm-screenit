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
import { Component, DestroyRef, DOCUMENT, inject, OnInit, signal } from '@angular/core'
import { interval, map, startWith } from 'rxjs'

import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { WINDOW } from '@shared/tokens/window.token'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'

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
  imports: [BaseDialogComponent, DsButtonComponent],
  templateUrl: './session-timeout.component.html',
  styleUrls: ['./session-timeout.component.scss'],
})
export class SessionTimeoutComponent implements OnInit {
  timer = signal('')
  private readonly interval = interval(1000)
  private readonly destroyRef = inject(DestroyRef)

  dialogData: { reset: () => void; logout: () => void; eindeTimeout: number } = inject(DIALOG_DATA)
  dialogRef = inject(DialogRef)
  document: Document = inject(DOCUMENT)
  window: Window = inject(WINDOW)

  ngOnInit() {
    this.interval
      .pipe(
        startWith(0),
        map(() => this.berekenTijdOver()),
        takeUntilDestroyed(this.destroyRef),
      )
      .subscribe((tijdOver) => {
        if (tijdOver <= 0) {
          this.dialogData.logout()
          return
        }

        const minuten = Math.floor(tijdOver / 60000)
        const seconden = Math.floor((tijdOver - minuten * 60000) / 1000)

        let minutenTekst = ''
        if (minuten > 0) {
          minutenTekst = `${minuten}${minuten > 1 ? ' minuten ' : ' minuut '}`
        }
        const secondenTekst = seconden > 1 ? 'seconden' : 'seconde'

        this.timer.set(`${minutenTekst}${seconden} ${secondenTekst}`)
        this.document.title = `ScreenIT - ${minuten}m${seconden}s (Uw sessie verloopt)`
      })
  }

  berekenTijdOver() {
    return this.dialogData.eindeTimeout - new Date().getTime()
  }

  keepAlive() {
    this.window.screenit.keepAliveCallback()
    this.document.title = 'ScreenIT'
    this.dialogData.reset()
    this.dialogRef.close()
  }
}
