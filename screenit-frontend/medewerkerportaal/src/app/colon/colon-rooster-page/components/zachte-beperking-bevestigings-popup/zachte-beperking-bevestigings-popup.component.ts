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

import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { DsButtonComponent } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-zachte-beperking-bevestigings-popup',
  imports: [BaseDialogComponent, DsButtonComponent],
  template: `
    <app-base-dialog titel="Bevestiging">
      <div body class="body">
        @for (message of dialogData.messages; track message) {
          <div>{{ message }}</div>
        }
        <p>Wilt u doorgaan?</p>
      </div>
      <div buttons class="btn-group">
        <button ds-button-secondary data-testid="button_zachte_beperking_nee" (click)="handleNo()">Nee</button>
        <button ds-button-primary data-testid="button_zachte_beperking_ja" (click)="handleYes()">Ja</button>
      </div>
    </app-base-dialog>
  `,
})
export class ZachteBeperkingBevestigingsPopupComponent {
  dialogData: { messages: string[] } = inject(DIALOG_DATA)
  private dialogRef: DialogRef<boolean> = inject(DialogRef)

  handleYes() {
    this.dialogRef.close(true)
  }

  handleNo() {
    this.dialogRef.close(false)
  }
}
