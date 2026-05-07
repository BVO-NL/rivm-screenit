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
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { Component, inject, input } from '@angular/core'
import { take } from 'rxjs'
import { DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective } from '@topicus-rgp-ds/web'

export { Component, inject, input } from '@angular/core'
export { take } from 'rxjs'
export { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
export { NotificationService } from '@shared/services/notification/notification.service'

@Component({
  selector: 'app-dense2-export',
  imports: [DsCardComponent, DsButtonComponent, DsFooterActionsRightDirective],
  template: `
    <ds-card cardTitle="Export">
      <div class="row">
        <div class="col-3">Bestandsnaam</div>
        <div class="col-9">{{ exportBestandsnaam() }}</div>
      </div>
      <ng-template ds-footer-actions-right>
        <button ds-button-primary (click)="export()">Download</button>
      </ng-template>
    </ds-card>
  `,
})
export class Dense2ExportComponent {
  exportBestandsnaam = input('')
  private dense2Service: Dense2Service = inject(Dense2Service)
  private notificationService: NotificationService = inject(NotificationService)

  export() {
    this.dense2Service
      .export(this.exportBestandsnaam())
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success('Bestand is gedownload')
      })
  }
}
