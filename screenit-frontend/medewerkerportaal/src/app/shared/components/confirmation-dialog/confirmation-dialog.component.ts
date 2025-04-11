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

import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'

@Component({
  selector: 'app-confirmation-dialog',
  imports: [BaseDialogComponent],
  template: `
    <app-base-dialog [titel]="dialogData.title">
      <div body class="body">{{ dialogData.body }}</div>
      <div buttons class="btn-group">
        <button type="button" class="btn" (click)="handleNo()">Nee</button>
        <button type="button" class="btn btn-primary" (click)="handleYes()">Ja</button>
      </div>
    </app-base-dialog>
  `,
  styles: [
    `
      .body {
        white-space: pre-wrap;
      }
    `,
  ],
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
