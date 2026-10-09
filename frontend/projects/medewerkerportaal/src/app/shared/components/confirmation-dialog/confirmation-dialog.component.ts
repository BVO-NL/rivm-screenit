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
import { DomSanitizer } from '@angular/platform-browser'
import { DsButtonComponent, DsIconComponent } from '@topicus-rgp-ds/web'
import { StringsService } from '@shared/strings'
import { saneerHtml } from '@shared/utils/html-utils'
import { faClose } from '@fortawesome/pro-light-svg-icons'

@Component({
  selector: 'app-confirmation-dialog',
  imports: [DsButtonComponent, DsIconComponent],
  templateUrl: './confirmation-dialog.component.html',
  styleUrl: './confirmation-dialog.component.scss',
})
export class ConfirmationDialogComponent {
  private dialogRef: DialogRef<boolean> = inject(DialogRef)
  dialogData: { title: string; body: string; neeButtonTekst?: string; jaButtonTekst?: string } = inject(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()
  protected titel = this.dialogData.title ? this.dialogData.title : this.dialogData.body
  protected body = this.titel !== this.dialogData.body ? saneerHtml(inject(DomSanitizer), this.dialogData.body) : undefined
  protected readonly faClose = faClose

  handleYes() {
    this.dialogRef.close(true)
  }

  handleNo() {
    this.dialogRef.close(false)
  }
}
