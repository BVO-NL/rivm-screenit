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
import { Component, inject, signal } from '@angular/core'
import { FormControl, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { formatDate } from '@shared/utils/date-utils'
import { valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { bsnValidator } from '@shared/validators/bsn/bsn.validator'
import { Dialog } from '@angular/cdk/dialog'
import { ExtraBeveiligdeOmgevingService } from '@/algemeen/extra-beveiligde-omgeving/services/extra-beveiligde-omgeving/extra-beveiligde-omgeving.service'
import { BezwaarClient } from '@shared/types/algemeen/bezwaar-client'
import { take } from 'rxjs'
import { DatePipe } from '@angular/common'
import { PdfViewerComponent } from '@shared/components/pdf-viewer/pdf-viewer.component'
import { BriefInzienDialogComponent } from '@/algemeen/components/brief-inzien-dialog/brief-inzien-dialog.component'
import {
  DsButtonComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsDatepickerComponent,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsInputComponent,
  DsNoDataRow,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { NL_DATE_FORMAT } from '@shared/constants'
import { faEye } from '@fortawesome/pro-light-svg-icons'
import { PageComponent } from '@shared/components/page/page.component'

@Component({
  selector: 'app-extra-beveiligde-omgeving-client-zoeken-page',
  imports: [
    ReactiveFormsModule,
    DatePipe,
    PdfViewerComponent,
    DsDatepickerComponent,
    DsInputComponent,
    DsTableComponent,
    DsColumnDef,
    DsRowComponent,
    DsRowDef,
    DsCellDef,
    DsCell,
    DsHeaderCellDef,
    DsHeaderRowDef,
    DsHeaderRowComponent,
    DsHeaderCell,
    DsButtonComponent,
    DsIconComponent,
    DsNoDataRow,
    PageComponent,
  ],
  templateUrl: './extra-beveiligde-omgeving-client-zoeken-page.component.html',
  styles: `
    .form-actions {
      display: flex;
      justify-content: flex-end;
    }

    .filter {
      width: var(--page-width-md);
    }
  `,
})
export class ExtraBeveiligdeOmgevingClientZoekenPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly dialogService = inject(Dialog)
  private readonly extraBeveiligdeOmgevingService = inject(ExtraBeveiligdeOmgevingService)
  protected readonly eyeIcon = faEye
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT

  formGroup = this.formBuilder.group({
    geboortedatum: this.formBuilder.control<Date | null>(null, [Validators.required, valideDatumValidator]),
    bsn: ['', [Validators.required, bsnValidator]],
  })
  maxGeboorteDatum = formatDate(new Date())
  clienten = signal<BezwaarClient[]>([])
  document: string | undefined | null
  displayedColumns = ['bsn', 'geboortedatum', 'inzien']

  get bsnCtrl(): FormControl {
    return this.formGroup.get('bsn') as FormControl
  }

  get geboortedatumCtrl(): FormControl {
    return this.formGroup.get('geboortedatum') as FormControl
  }

  clientZoeken() {
    if (this.formGroup.invalid) {
      return
    }

    const formValue = this.formGroup.getRawValue()
    this.extraBeveiligdeOmgevingService
      .getClienten(formValue.bsn, formatDate(formValue.geboortedatum!))
      .pipe(take(1))
      .subscribe((response: BezwaarClient[]) => {
        this.clienten.set(response)
      })
  }

  briefOpenen(client: BezwaarClient) {
    this.dialogService.open(BriefInzienDialogComponent, {
      data: client.briefDocument,
      panelClass: 'pdf-inzien-dialog',
    })
  }
}
