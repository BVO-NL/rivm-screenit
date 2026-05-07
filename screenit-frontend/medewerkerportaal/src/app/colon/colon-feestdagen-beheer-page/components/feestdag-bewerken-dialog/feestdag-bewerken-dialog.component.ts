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
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ColonFeestdag } from '@shared/types/colon/colon-feestdag'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'
import { ColonRoosterBeperkingComponent } from '@/colon/components/colon-rooster-beperking/colon-rooster-beperking.component'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsDatepickerComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { formatDate } from '@shared/utils/date-utils'

@Component({
  selector: 'app-feestdag-bewerken-dialog',
  imports: [FormsModule, ColonRoosterBeperkingComponent, ReactiveFormsModule, BaseDialogComponent, DsInputComponent, DsDatepickerComponent, DsButtonComponent],
  templateUrl: './feestdag-bewerken-dialog.component.html',
})
export class FeestdagBewerkenDialogComponent {
  private readonly dialogData: ColonFeestdag = inject(DIALOG_DATA)
  private readonly feestdag: ColonFeestdag = this.getFeestdag()
  private readonly dialogRef: DialogRef = inject(DialogRef)
  private readonly formBuilder: FormBuilder = inject(FormBuilder)
  protected readonly minDatum = formatDate(new Date())

  feestdagForm: FormGroup = this.formBuilder.group({
    naam: ['', Validators.required],
    datum: ['', { validators: [Validators.required] }],
    beperking: [ColonRoosterBeperking.HARD, Validators.required],
  })

  get modalTitle(): string {
    return this.feestdag?.id ? 'Feestdag bewerken' : 'Feestdag toevoegen'
  }

  constructor() {
    if (this.feestdag) {
      this.feestdagForm.patchValue(this.feestdag)
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
    const feestdag = {
      ...this.feestdag,
      ...formValue,
      actief: true,
    }

    this.dialogRef.close(feestdag)
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
