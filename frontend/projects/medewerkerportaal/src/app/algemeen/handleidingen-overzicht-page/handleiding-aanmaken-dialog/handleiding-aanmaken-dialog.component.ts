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
import { FormArray, FormBuilder, FormControl, ReactiveFormsModule } from '@angular/forms'
import { DialogRef } from '@angular/cdk/dialog'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsFileModel, DsFileUploaderComponent, DsFileUploaderFileComponent, DsFileUploaderOptionsModel, DsInputComponent } from '@topicus-rgp-ds/web'
import { HandleidingenService } from '@/algemeen/handleidingen-overzicht-page/services/handleidingen.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { take } from 'rxjs'
import { haalBestandsnaamZonderExtensie, maakDSFileModel } from '@/algemeen/handleidingen-overzicht-page/utils/handleidingen-util'
import { HandleidingUpload } from '@shared/utils/file-utils'
import { Component, inject } from '@angular/core'

@Component({
  selector: 'app-handleiding-aanmaken-dialog',
  imports: [ReactiveFormsModule, BaseDialogComponent, DsButtonComponent, DsFileUploaderComponent, DsFileUploaderFileComponent, DsInputComponent],
  templateUrl: './handleiding-aanmaken-dialog.component.html',
})
export class HandleidingAanmakenDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly handleidingenService = inject(HandleidingenService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder: FormBuilder = inject(FormBuilder)

  readonly handleidingForm = this.formBuilder.group({
    handleidingenUpload: this.formBuilder.control<File[]>([], { nonNullable: true }),
    bestandsnamen: this.formBuilder.array<FormControl<string>>([]),
  })

  readonly fileUploaderOptions = new DsFileUploaderOptionsModel({
    multiple: true,
    inline: true,
    acceptedFileFormats: '.pdf',
  })

  constructor() {
    this.handleidingForm.controls.handleidingenUpload.valueChanges.pipe(takeUntilDestroyed()).subscribe((bestanden) => {
      bestanden.forEach((bestand, index) => {
        if (index >= this.bestandsnamen.length) {
          const control = this.formBuilder.nonNullable.control(haalBestandsnaamZonderExtensie(bestand))
          control.markAsTouched()
          this.bestandsnamen.push(control)
        }
      })
    })
  }

  get bestandsnamen() {
    return this.handleidingForm.get('bestandsnamen') as FormArray<FormControl<string>>
  }

  get handleidingen(): DsFileModel[] {
    return this.handleidingForm.controls.handleidingenUpload.value.map(maakDSFileModel)
  }

  get isFormGeldig(): boolean {
    return this.handleidingen.length > 0 && this.bestandsnamen.valid
  }

  annuleren(): void {
    this.dialogRef.close()
  }

  opslaan(): void {
    if (!this.isFormGeldig) {
      return
    }
    const uploadItems: HandleidingUpload[] = this.handleidingForm.controls.handleidingenUpload.value.map((bestand, index) => ({
      bestand,
      bestandsnaam: this.bestandsnamen.at(index).value,
    }))
    this.handleidingenService
      .uploadHandleidingen(uploadItems)
      .pipe(take(1))
      .subscribe((resultaten) => {
        const nietGeslaagd = resultaten.filter((r) => !r.geslaagd)
        if (nietGeslaagd.length === 0) {
          this.notificationService.success('Alle handleidingen zijn succesvol geüpload.')
        } else {
          nietGeslaagd.forEach((r) => this.notificationService.error(r.foutmelding!))
        }
        this.dialogRef.close(true)
      })
  }

  verwijderHandleidingVanLijst(bestand: DsFileModel): void {
    const index = this.handleidingForm.controls.handleidingenUpload.value.findIndex((f) => f.name === bestand.name)
    if (index !== -1) {
      this.bestandsnamen.removeAt(index)
      this.handleidingForm.controls.handleidingenUpload.setValue(this.handleidingForm.controls.handleidingenUpload.value.filter((f) => f.name !== bestand.name))
    }
  }
}
