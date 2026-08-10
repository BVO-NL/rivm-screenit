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
import { Component, inject } from '@angular/core'
import { NotificationService } from '@shared/services/notification/notification.service'
import { FormsModule } from '@angular/forms'
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
import { take } from 'rxjs'
import { DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective } from '@topicus-rgp-ds/web'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'

@Component({
  selector: 'app-dense2-import',
  imports: [FormsModule, DsCardComponent, DsButtonComponent, DsFooterActionsRightDirective, SingleFileSelectorComponent],
  template: `
    <ds-card cardTitle="Import">
      <app-single-file-selector class="display-block" acceptedExtensie="csv" [(ngModel)]="file" />
      <ng-template ds-footer-actions-right>
        <button ds-button-primary data-testid="button_import" [disabled]="!file" (click)="import()">Verwerken</button>
      </ng-template>
    </ds-card>
  `,
})
export class Dense2ImportComponent {
  file: File | undefined
  private notificationService: NotificationService = inject(NotificationService)
  private dense2Service: Dense2Service = inject(Dense2Service)

  import() {
    if (!this.file) {
      return
    }

    this.dense2Service
      .import(this.file)
      .pipe(take(1))
      .subscribe({
        next: (melding) => this.notificationService.success(melding),
        error: (err) => {
          if (err.status === 403) {
            this.notificationService.error('Type bestand kan niet verwerkt worden.')
          }
        },
      })
  }
}
