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

import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ColonBulkVerwijderenException } from '@shared/types/colon/colon-bulk-verwijderen-exception'
import { SelectOption } from '@shared/types/select-option'
import { LowerCasePipe } from '@angular/common'

@Component({
  selector: 'app-tijdslots-verwijderen-bevestigings-popup',
  imports: [BaseDialogComponent, LowerCasePipe],
  styles: [
    `
      td {
        text-align: left;
        &:first-child {
          width: 200px;
        }
      }

      .table {
        margin-top: 0;
        width: 300px;
      }
    `,
  ],
  templateUrl: './tijdslots-verwijderen-bevestigings-popup.component.html',
})
export class TijdslotsVerwijderenBevestigingsPopupComponent {
  private dialogRef: DialogRef<boolean> = inject(DialogRef)
  dialogData: { bulkVerwijderenResultaat: ColonBulkVerwijderenException; typeTijdslot: SelectOption<string> } = inject(DIALOG_DATA)

  handleYes() {
    this.dialogRef.close(true)
  }

  handleNo() {
    this.dialogRef.close(false)
  }
}
