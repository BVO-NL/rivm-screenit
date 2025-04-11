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
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'
import { ColonBulkAanmakenException } from '@shared/types/colon/colon-bulk-aanmaken-exception'
import { BulkAanmakenResultatenTabelComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-resultaten-tabel/bulk-aanmaken-resultaten-tabel.component'

@Component({
  selector: 'app-bulk-aanmaken-bevestigings-popup',
  imports: [BaseDialogComponent, BulkAanmakenResultatenTabelComponent],
  templateUrl: './bulk-aanmaken-bevestigings-popup.component.html',
})
export class BulkAanmakenBevestigingsPopupComponent {
  private dialogRef: DialogRef<boolean> = inject(DialogRef)
  dialogData: { exceptions: ColonBulkAanmakenException[]; typeTijdslot: string } = inject(DIALOG_DATA)
  exceptions: { hard: ColonBulkAanmakenException[]; zacht: ColonBulkAanmakenException[] } = { hard: [], zacht: [] }

  get heeftZachteBeperkingen(): boolean {
    return this.exceptions.zacht.length > 0
  }

  get heeftHardeBeperkingen(): boolean {
    return this.exceptions.hard.length > 0
  }

  constructor() {
    this.dialogData.exceptions.forEach((exception) => {
      if (exception.type === ColonRoosterBeperking.HARD) {
        this.exceptions.hard.push(exception)
      } else {
        this.exceptions.zacht.push(exception)
      }
    })
  }

  handleYes() {
    this.dialogRef.close(true)
  }

  handleNo() {
    this.dialogRef.close(false)
  }
}
