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
import { Component, inject } from '@angular/core'

import { ClrAlertModule } from '@clr/angular'
import { animate, style, transition, trigger } from '@angular/animations'
import { ToastService } from '@shared/toast/service/toast.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { debounce, filter, timer } from 'rxjs'
import { SanitizePipe } from '@shared/pipes/sanitize/sanitize.pipe'
import { Toast } from '@shared/types/toast'

@Component({
  selector: 'app-toast',
  imports: [ClrAlertModule, SanitizePipe],
  templateUrl: './toast.component.html',
  styleUrls: ['./toast.component.scss'],
  animations: [
    trigger('fadeSlideInOut', [
      transition(':enter', [style({ opacity: 0, transform: 'translateY(10px)' }), animate('200ms', style({ opacity: 1, transform: 'translateY(0)' }))]),
      transition(':leave', [animate('200ms', style({ opacity: 0, transform: 'translateY(10px)' }))]),
    ]),
  ],
})
export class ToastComponent {
  toasts: Toast[] = []
  private toastService: ToastService = inject(ToastService)
  private updateDelay = 300

  constructor() {
    this.toastService.showToast$
      .pipe(
        takeUntilDestroyed(),
        filter((toast) => !this.hasToast(toast)),
        debounce(() => timer(this.updateDelay)),
      )
      .subscribe((toast: Toast) => {
        this.toasts.push(toast)

        if (toast.autoClose) {
          setTimeout(() => {
            this.closeToast(toast.id)
          }, toast.autoClose + this.updateDelay)
        }
      })

    this.toastService.hideToast$
      .pipe(
        takeUntilDestroyed(),
        debounce(() => timer(this.updateDelay)),
      )
      .subscribe((message: string) => {
        const index = this.toasts.findIndex((toast) => toast.message === message)
        if (index !== -1) {
          this.toasts.splice(index, 1)
        }
      })

    this.toastService.clearToasts$.pipe(takeUntilDestroyed()).subscribe(() => {
      this.toasts = []
    })
  }

  closeToast(id: number) {
    const index = this.toasts.findIndex((toast) => toast.id === id)
    if (index !== -1) {
      this.toasts.splice(index, 1)
    }
  }

  private hasToast(toast: Toast): boolean {
    return this.toasts.find((_toast) => _toast.message === toast.message) != undefined
  }
}
