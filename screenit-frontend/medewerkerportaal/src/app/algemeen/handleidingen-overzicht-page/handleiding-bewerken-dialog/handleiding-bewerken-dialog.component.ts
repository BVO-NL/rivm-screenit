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
import { FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule } from '@angular/forms'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsFileModel, DsInputComponent } from '@topicus-rgp-ds/web'
import { MatFormFieldModule } from '@angular/material/form-field'
import { MatInputModule } from '@angular/material/input'
import { HandleidingenService } from '@/algemeen/handleidingen-overzicht-page/services/handleidingen.service'
import { HandleidingBeheren } from '@shared/types/algemeen/handleiding'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { take } from 'rxjs'
import { haalBestandsnaamZonderExtensie, maakDSFileModel } from '@/algemeen/handleidingen-overzicht-page/utils/handleidingen-util'
import { NotificationService } from '@shared/services/notification/notification.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'

@Component({
  selector: 'app-handleiding-bewerken-dialog',
  imports: [FormsModule, ReactiveFormsModule, BaseDialogComponent, DsInputComponent, DsButtonComponent, MatFormFieldModule, MatInputModule, SingleFileSelectorComponent],
  templateUrl: './handleiding-bewerken-dialog.component.html',
})
export class HandleidingBewerkenDialogComponent {
  private readonly dialogData: HandleidingBeheren = inject(DIALOG_DATA)
  private readonly dialogRef: DialogRef = inject(DialogRef)
  private readonly formBuilder: FormBuilder = inject(FormBuilder)
  private readonly handleidingenService: HandleidingenService = inject(HandleidingenService)
  private readonly notificationService = inject(NotificationService)
  private geselecteerdBestand: File | null = null

  geselecteerdBestandFileModel: DsFileModel | null = null
  handleidingForm: FormGroup = this.formBuilder.group({
    bestand: null,
    bestandsnaam: new FormControl(this.dialogData.bestandsnaam),
  })

  constructor() {
    this.handleidingForm
      .get('bestand')!
      .valueChanges.pipe(takeUntilDestroyed())
      .subscribe((bestand: File) => {
        this.verwerkHandleiding(bestand)
      })
    this.bestandsnaamControl.markAsTouched()
  }

  get bestandsnaamControl(): FormControl {
    return this.handleidingForm.get('bestandsnaam') as FormControl
  }

  get isBestandGewijzigd(): boolean {
    return this.geselecteerdBestand !== null
  }

  get isBestandsnaamGewijzigd(): boolean {
    return this.bestandsnaamControl.value !== this.dialogData.bestandsnaam
  }

  get isFormGeldig(): boolean {
    return this.handleidingForm.valid && (this.isBestandGewijzigd || this.isBestandsnaamGewijzigd)
  }

  annuleren(): void {
    this.dialogRef.close()
  }

  opslaan(): void {
    if (!this.isFormGeldig) {
      return
    }
    const bestandsnaam = this.bestandsnaamControl.value || this.dialogData.bestandsnaam
    this.handleidingenService
      .bewerkHandleiding(this.dialogData.id, this.geselecteerdBestand, bestandsnaam)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success(`${bestandsnaam} is succesvol bijgewerkt.`)
          this.dialogRef.close(true)
        },
        error: (error) => {
          const foutmelding = error.error.foutmelding
          this.notificationService.error(foutmelding)
          this.dialogRef.close()
        },
      })
  }

  private verwerkHandleiding(bestand: File): void {
    this.geselecteerdBestand = bestand
    if (bestand) {
      this.geselecteerdBestandFileModel = maakDSFileModel(bestand)
      this.bestandsnaamControl.setValue(haalBestandsnaamZonderExtensie(bestand))
    } else {
      this.geselecteerdBestandFileModel = null
    }
  }
}
