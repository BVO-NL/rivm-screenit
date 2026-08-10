/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { DsButtonComponent, DsCheckboxComponent, DsFooterActionsRightDirective, DsInputComponent, DsModalComponent, DsModalConfig } from '@topicus-rgp-ds/web'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { LocatieService } from '../../services/locatie/locatie.service'
import { LocatieDto, LocatieStatus } from '../../models/LocatieDto'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { CustomValidators } from '../../utils/custom-validators'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { WoonplaatsSelectorComponent } from '../woonplaats-selector/woonplaats-selector.component'
import { AdresDto } from '../../models/AdresDto'
import { WoonplaatsDto } from '../../models/WoonplaatsDto'
import { take } from 'rxjs'

@Component({
  selector: 'app-locatie-edit-dialog',
  imports: [DsFooterActionsRightDirective, DsButtonComponent, DsModalComponent, ReactiveFormsModule, DsInputComponent, DsCheckboxComponent, WoonplaatsSelectorComponent],
  templateUrl: './locatie-bewerken-dialog.component.html',
  styleUrl: './locatie-bewerken-dialog.component.scss',
})
export class LocatieBewerkenDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly locatieService = inject(LocatieService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialogData: { locatie: LocatieDto; postAdres: AdresDto } = inject(DIALOG_DATA)

  get isNew(): boolean {
    return this.locatieForm.get('huisartsportaalId')?.value != null
  }

  locatieForm = this.formBuilder.group({
    huisartsportaalId: this.formBuilder.control<number | null>(null),
    iban: ['', [Validators.required, CustomValidators.ibanValidator]],
    ibanTenaamstelling: ['', Validators.required],
    naam: ['', Validators.required],
    locatieAdres: this.formBuilder.group({
      straat: ['', Validators.required],
      huisnummer: this.formBuilder.control<number | null>(null, [Validators.required]),
      huisnummertoevoeging: this.formBuilder.control<string | null>(null),
      postcode: ['', Validators.required],
      woonplaats: this.formBuilder.control<WoonplaatsDto | null>(null, [Validators.required]),
    }),
    status: LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD,
    zorgmailklantnummer: ['', [Validators.required, Validators.maxLength(9), Validators.minLength(9)]],
  })
  gelijkAanNawCtrl = this.formBuilder.control<boolean>(false)

  locatieEditModalConfig: DsModalConfig = {
    closeCallback: () => {
      this.dialogRef.close()
      return true
    },
    contentPadding: '1rem',
    preventBackdropClose: true,
    minWidth: '70vw',
    maxWidth: '90vw',
    minHeight: '450px',
  }

  constructor() {
    if (this.dialogData?.locatie) {
      this.locatieForm.patchValue(this.dialogData.locatie)
    }
    this.gelijkAanNawCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((value) => {
      const locatieAdresGroup = this.locatieForm.get('locatieAdres') as FormControl
      if (value) {
        locatieAdresGroup.disable()
        if (this.dialogData.postAdres) {
          locatieAdresGroup.patchValue(this.dialogData.postAdres)
        }
      } else {
        locatieAdresGroup.enable()
      }
    })
  }

  annuleren() {
    this.dialogRef.close(false)
  }

  opslaan() {
    if (this.locatieForm.invalid) {
      return
    }

    this.locatieService
      .updateLocatie(this.locatieForm.getRawValue() as unknown as LocatieDto)
      .pipe(take(1))
      .subscribe(() => this.dialogRef.close(true))
  }
}
