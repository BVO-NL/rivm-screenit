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
import { MammaVisitatielijstResponseDto } from '@shared/types/mamma/dto/visitatie/mamma-visitatielijst-response.dto'
import { MammaVisitatieService } from '@/mamma/mamma-visitatie-overzicht-page/services/mamma-visitatie.service'
import { take } from 'rxjs'
import writeXlsxFile from 'write-excel-file/browser'
import { MammaVisitatielijstDialogData } from '@shared/types/mamma/dto/visitatie/mamma-visitatielijst-dialog-data'
import { MammaVisitatielijstRapport } from '@shared/types/mamma/mamma-visitatielijst-rapport'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { NotificationService } from '@shared/services/notification/notification.service'

@Component({
  selector: 'app-mamma-visitatielijst-rapport-dialog',
  imports: [BaseDialogComponent, DsButtonComponent],
  templateUrl: './mamma-visitatielijst-rapport-dialog.component.html',
  styles: `
    .section-header {
      font-weight: bold;
      font-size: 16px;
    }
  `,
})
export class MammaVisitatielijstRapportDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly dialogData: MammaVisitatielijstDialogData = inject(DIALOG_DATA)
  private readonly visitatieService = inject(MammaVisitatieService)
  private readonly notificationService = inject(NotificationService)
  protected rapport = new MammaVisitatielijstRapport(this.dialogData.rapport)

  sluitDialog() {
    this.dialogRef.close()
  }

  async exporteren() {
    const toCell = (value: string | number | null | undefined) => ({ value: value ?? undefined })

    const rows = [
      [toCell('aantalMedewerkersVerwerkt'), toCell('aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden'), toCell('aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden')],
      [
        toCell(this.rapport.aantalMedewerkersVerwerkt),
        toCell(this.rapport.aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden),
        toCell(this.rapport.aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden),
      ],
      [],
      [toCell('Overzicht van medewerkers met te weinig beelden')],
      [toCell('medewerkercode'), toCell('aantalBeelden')],
      ...this.rapport.uitgeslotenMedewerkerIds.map((medewerker) => [toCell(medewerker.medewerkercode), toCell(medewerker.aantal)]),
      [],
      [toCell('Overzicht resultaten per medewerker')],
      [toCell('medewerkercode'), toCell('aantalBeeldenBinnen2Maanden'), toCell('aantalBeeldenBinnen4Maanden')],
      ...this.rapport.aantalOnderzoekenLijst.map((aantal) => [
        toCell(aantal.medewerkercode),
        toCell(aantal.aantalBeeldenBinnen2Maanden),
        toCell(aantal.aantalBeeldenBinnen4Maanden),
      ]),
    ]

    await writeXlsxFile(rows).toFile('rapport.xlsx')
  }

  genereren() {
    this.visitatieService
      .genereerVisitatieLijst(this.dialogData.request, this.dialogData.bestand, false)
      .pipe(take(1))
      .subscribe((response: MammaVisitatielijstResponseDto) => {
        if (response.meldingen.length) {
          this.notificationService.warning(response.meldingen.join(', '))
        }
        this.dialogRef.close(response.visitaties)
      })
  }
}
