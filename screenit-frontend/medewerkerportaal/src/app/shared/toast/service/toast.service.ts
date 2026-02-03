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
import { Injectable } from '@angular/core'
import { Subject } from 'rxjs'
import { Toast } from '@shared/types/toast'

@Injectable({
  providedIn: 'root',
})
export class ToastService {
  showToast$ = new Subject<Toast>()
  hideToast$ = new Subject<string | string[]>()
  clearToasts$ = new Subject<void>()

  error(message: string | string[]) {
    this.showToast({
      id: Math.random(),
      message,
      type: 'danger',
      disableClose: false,
    })
  }

  success(message: string | string[]) {
    this.showToast({
      id: Math.random(),
      message,
      type: 'success',
      disableClose: false,
      autoClose: 5000,
    })
  }

  warning(message: string | string[]) {
    this.showToast({
      id: Math.random(),
      message,
      type: 'warning',
      disableClose: false,
      autoClose: 5000,
    })
  }

  hide(message: string | string[]) {
    this.hideToast$.next(message)
  }

  clear() {
    this.clearToasts$.next()
  }

  private showToast(toast: Toast) {
    this.showToast$.next(toast)
  }
}
