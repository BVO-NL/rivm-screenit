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

import { ClrCheckboxModule, ClrDatepickerModule, ClrFormsModule, ClrInputModule, ClrModalModule } from '@clr/angular'
import { FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ColonFeestdag } from '@shared/types/colon/colon-feestdag'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'
import { FeestdagenService } from '@/colon/colon-feestdagen-beheer-page/services/feestdagen.service'
import { iif, take } from 'rxjs'
import { ColonRoosterBeperkingComponent } from '@/colon/components/colon-rooster-beperking/colon-rooster-beperking.component'
import { datumInVerledenValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { format } from 'date-fns'
import { NL_DATE_FORMAT } from '@shared/constants'
import { formatDate, formatNLDate, parseDate } from '@shared/date-utils'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'

@Component({
  selector: 'app-feestdag-edit-dialog',
  imports: [
    ClrModalModule,
    ClrFormsModule,
    ClrDatepickerModule,
    ClrInputModule,
    FormsModule,
    ClrCheckboxModule,
    ColonRoosterBeperkingComponent,
    ReactiveFormsModule,
    BaseDialogComponent,
  ],
  templateUrl: './feestdag-edit-dialog.component.html',
})
export class FeestdagEditDialogComponent {
  private formBuilder: FormBuilder = inject(FormBuilder)
  feestdagenService: FeestdagenService = inject(FeestdagenService)
  dialogData: ColonFeestdag = inject(DIALOG_DATA)
  feestdag: ColonFeestdag = this.getFeestdag()
  dialogRef: DialogRef = inject(DialogRef)
  dialog: Dialog = inject(Dialog)
  now = formatDate(new Date())
  feestdagForm: FormGroup = this.formBuilder.group({
    feestdagNaam: ['', Validators.required],
    feestdagDatum: [formatNLDate(new Date()), { validators: [Validators.required, datumInVerledenValidator, valideDatumValidator], updateOn: 'blur' }],
    feestdagBeperking: [ColonRoosterBeperking.HARD, Validators.required],
  })

  get datumCtrl(): FormControl {
    return this.feestdagForm.get('feestdagDatum') as FormControl
  }

  get modalTitle(): string {
    return this.feestdag?.id ? 'Feestdag bewerken' : 'Feestdag toevoegen'
  }

  constructor() {
    if (this.feestdag) {
      this.feestdagForm.patchValue({
        ...this.feestdag,
        feestdagNaam: this.feestdag.naam,
        feestdagDatum: format(this.feestdag.datum, NL_DATE_FORMAT),
        feestdagBeperking: this.feestdag.beperking,
      })
    }
  }

  cancel() {
    this.dialogRef.close()
  }

  save() {
    if (this.feestdagForm.invalid) {
      return
    }

    const formValue = this.feestdagForm.getRawValue()
    const datum = parseDate(formValue.feestdagDatum)
    const feestdag = {
      ...this.feestdag,
      naam: formValue.feestdagNaam,
      datum,
      beperking: formValue.feestdagBeperking,
      actief: true,
    }

    iif(() => feestdag.id == null, this.feestdagenService.createFeestdag(feestdag), this.feestdagenService.updateFeestdag(feestdag))
      .pipe(take(1))
      .subscribe(() => {
        this.dialogRef.close(feestdag)
      })
  }

  private getFeestdag(): ColonFeestdag {
    return this.dialogData
      ? { ...this.dialogData }
      : {
          naam: '',
          datum: new Date(),
          beperking: ColonRoosterBeperking.HARD,
          actief: true,
        }
  }
}
