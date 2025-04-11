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
import { Component, EventEmitter, Input, Output } from '@angular/core'

import { ClrModalModule } from '@clr/angular'

@Component({
  selector: 'app-base-dialog',
  imports: [ClrModalModule],
  template: ` <clr-modal [clrModalOpen]="true" [clrModalSize]="size" (clrModalOpenChange)="sluiten.emit()">
    <h3 class="modal-title">{{ titel }}</h3>
    <div class="modal-body">
      <ng-content select="[body]"></ng-content>
    </div>
    <div class="modal-footer" [class.justify-buttons]="justifyButtons">
      <ng-content select="[buttons]"></ng-content>
    </div>
  </clr-modal>`,
  styles: [
    `
      .modal-footer {
        display: flex;
        justify-content: end;
        &.justify-buttons {
          justify-content: space-between;
        }
        width: 100%;
      }
    `,
  ],
})
export class BaseDialogComponent {
  @Output() sluiten: EventEmitter<void> = new EventEmitter()
  @Input({ required: true }) titel = 'Titel'
  @Input() justifyButtons = false
  @Input() size = 'md'
}
