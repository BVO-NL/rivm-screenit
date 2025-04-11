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
import { Component, inject, Signal, signal } from '@angular/core'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { PdfViewerComponent } from '@shared/components/pdf-viewer/pdf-viewer.component'
import { DocumentenService } from '@shared/services/documenten/documenten.service'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { saveAs } from 'file-saver'

@Component({
  selector: 'app-brief-inzien-dialog',
  imports: [BaseDialogComponent, PdfViewerComponent],
  templateUrl: './brief-inzien-dialog.component.html',
})
export class BriefInzienDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly documentenService = inject(DocumentenService)
  private readonly documentId: number = inject(DIALOG_DATA)
  brief: Signal<string | undefined> = signal(undefined)

  constructor() {
    this.brief = toSignal(this.documentenService.getById(this.documentId).pipe(takeUntilDestroyed()))
  }

  sluiten() {
    this.dialogRef.close()
  }

  opslaan() {
    if (this.brief()) {
      saveAs(this.brief()!, 'brief.pdf')
    }
  }
}
