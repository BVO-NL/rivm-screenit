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
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
import { ToastService } from '@shared/toast/service/toast.service'
import { Component, inject, input } from '@angular/core'
import { CardComponent } from '@shared/components/card/card.component'
import { take } from 'rxjs'

export { Component, inject, input } from '@angular/core'
export { CardComponent } from '@shared/components/card/card.component'
export { take } from 'rxjs'
export { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
export { ToastService } from '@shared/toast/service/toast.service'

@Component({
  selector: 'app-dense2-export',
  imports: [CardComponent],
  template: `
    <app-card>
      <div header>Export</div>
      <div body class="clr-row">
        <div class="clr-col-3">Bestandsnaam</div>
        <div class="clr-col-9">{{ exportBestandsnaam() }}</div>
      </div>
      <button footer class="btn btn-primary" (click)="export()">Download</button>
    </app-card>
  `,
})
export class Dense2ExportComponent {
  exportBestandsnaam = input('')
  private dense2Service: Dense2Service = inject(Dense2Service)
  private toastService: ToastService = inject(ToastService)

  export() {
    this.dense2Service
      .export(this.exportBestandsnaam())
      .pipe(take(1))
      .subscribe(() => {
        this.toastService.success('Bestand is gedownload')
      })
  }
}
