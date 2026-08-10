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
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { KeyValuePipe } from '@angular/common'

@Component({
  selector: 'app-feestdag-foutmelding-popup',
  imports: [BaseDialogComponent, DsButtonComponent, KeyValuePipe],
  templateUrl: './feestdag-foutmelding-popup.component.html',
  styles: `
    .ds-table .ds-cell {
      vertical-align: top;
      line-height: 1.25rem;
    }
  `,
})
export class FeestdagFoutmeldingPopupComponent {
  private readonly dialogRef = inject(DialogRef)
  protected readonly error: { message: string; afspraakslots: { [intakelocatie: string]: string[] } } = inject(DIALOG_DATA)

  sluiten() {
    this.dialogRef.close()
  }

  protected readonly Object = Object
}
