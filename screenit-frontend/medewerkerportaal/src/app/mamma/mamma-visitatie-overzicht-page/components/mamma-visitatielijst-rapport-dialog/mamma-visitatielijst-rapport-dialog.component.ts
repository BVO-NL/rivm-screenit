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
import { ClrButtonModule } from '@clr/angular'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { MammaVisitatielijstResponseDto } from '@shared/types/mamma/dto/mamma-visitatielijst-response.dto'
import { MammaVisitatieService } from '@/mamma/mamma-visitatie-overzicht-page/services/mamma-visitatie.service'
import { take } from 'rxjs'
import { ToastService } from '@shared/toast/service/toast.service'
import * as ExcelJS from 'exceljs'
import { saveAs } from 'file-saver'
import { MammaVisitatielijstDialogData } from '@shared/types/mamma/dto/mamma-visitatielijst-dialog-data'
import { MammaVisitatielijstRapport } from '@shared/types/mamma/mamma-visitatielijst-rapport'

@Component({
  selector: 'app-mamma-visitatielijst-rapport-dialog',
  imports: [BaseDialogComponent, ClrButtonModule],
  templateUrl: './mamma-visitatielijst-rapport-dialog.component.html',
})
export class MammaVisitatielijstRapportDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly dialogData: MammaVisitatielijstDialogData = inject(DIALOG_DATA)
  private readonly visitatieService = inject(MammaVisitatieService)
  private readonly toastService = inject(ToastService)
  protected rapport = new MammaVisitatielijstRapport(this.dialogData.rapport)

  sluitDialog() {
    this.dialogRef.close()
  }

  async exporteren() {
    const workbook = new ExcelJS.Workbook()
    const worksheet = workbook.addWorksheet('Rapport')

    worksheet.addRow(['aantalMedewerkersVerwerkt', 'aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden', 'aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden'])
    worksheet.addRow([
      this.rapport.aantalMedewerkersVerwerkt,
      this.rapport.aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden,
      this.rapport.aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden,
    ])
    worksheet.addRow([])

    worksheet.addRow(['Overzicht van medewerkers met te weinig beelden'])
    worksheet.addRow(['medewerkercode', 'aantalBeelden'])
    this.rapport.uitgeslotenMedewerkerIds.forEach((m) => worksheet.addRow([m.medewerkercode, m.aantal]))
    worksheet.addRow([])

    worksheet.addRow(['Overzicht resultaten per medewerker'])
    worksheet.addRow(['medewerkercode', 'aantalBeeldenBinnen2Maanden', 'aantalBeeldenBinnen4Maanden'])
    this.rapport.aantalOnderzoekenLijst.forEach((aantal) => worksheet.addRow([aantal.medewerkercode, aantal.aantalBeeldenBinnen2Maanden, aantal.aantalBeeldenBinnen4Maanden]))

    const buffer = await workbook.xlsx.writeBuffer()
    saveAs(new Blob([buffer]), 'rapport.xlsx')
  }

  genereren() {
    this.visitatieService
      .genereerVisitatieLijst(this.dialogData.request, this.dialogData.bestand, false)
      .pipe(take(1))
      .subscribe((response: MammaVisitatielijstResponseDto) => {
        if (response.meldingen.length) {
          this.toastService.warning(response.meldingen)
        }
        this.dialogRef.close(response.visitaties)
      })
  }
}
