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

import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-confirmation-dialog',
  imports: [BaseDialogComponent, DsButtonComponent],
  template: `
    <app-base-dialog [titel]="dialogData.title">
      <div body>{{ dialogData.body }}</div>
      <div buttons class="btn-group">
        <button (click)="handleNo()" data-testid="button_confirmation_nee" ds-button-secondary type="button">Nee</button>
        <button (click)="handleYes()" data-testid="button_confirmation_ja" ds-button-primary type="button">Ja</button>
      </div>
    </app-base-dialog>
  `,
})
export class ConfirmationDialogComponent {
  private dialogRef: DialogRef<boolean> = inject(DialogRef)
  dialogData: { title: string; body: string } = inject(DIALOG_DATA)

  handleYes() {
    this.dialogRef.close(true)
  }

  handleNo() {
    this.dialogRef.close(false)
  }
}
