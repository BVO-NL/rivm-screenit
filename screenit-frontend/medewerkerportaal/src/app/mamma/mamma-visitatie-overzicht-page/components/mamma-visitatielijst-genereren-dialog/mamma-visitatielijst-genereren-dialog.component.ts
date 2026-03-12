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
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { ClrFileInputModule, ClrInputModule } from '@clr/angular'
import { MammaVisitatieService } from '@/mamma/mamma-visitatie-overzicht-page/services/mamma-visitatie.service'
import { aantalBestandenValidator, extensieValidator } from '@shared/validators/file/file.validator'
import { ContentType } from '@shared/types/content-type'
import { take } from 'rxjs'
import { DialogRef } from '@angular/cdk/dialog'
import { MammaVisitatielijstResponseDto } from '@shared/types/mamma/dto/mamma-visitatielijst-response.dto'
import { ToastService } from '@shared/toast/service/toast.service'

@Component({
  selector: 'app-mamma-visitatielijst-genereren-dialog',
  imports: [BaseDialogComponent, ReactiveFormsModule, ClrInputModule, ClrFileInputModule],
  templateUrl: './mamma-visitatielijst-genereren-dialog.component.html',
})
export class MammaVisitatielijstGenererenDialogComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly visitatieService = inject(MammaVisitatieService)
  private readonly dialogRef = inject(DialogRef)
  private readonly toastService = inject(ToastService)
  protected contentType = ContentType

  genererenForm = this.formBuilder.group({
    omschrijving: ['', Validators.required],
    bestand: this.formBuilder.control<FileList | null>(null, [Validators.required, extensieValidator(['csv']), aantalBestandenValidator(1)]),
  })

  protected get bestandCtrl(): FormControl {
    return this.genererenForm.get('bestand') as FormControl
  }

  protected get omschrijvingCtrl(): FormControl {
    return this.genererenForm.get('omschrijving') as FormControl
  }

  sluitDialog() {
    this.dialogRef.close()
  }

  rapportGenereren() {
    if (this.genererenForm.invalid) {
      return
    }

    const request = { omschrijving: this.omschrijvingCtrl.value as string }
    const bestand = this.bestandCtrl.value[0]
    this.visitatieService
      .genereerVisitatieLijst(request, bestand, true)
      .pipe(take(1))
      .subscribe((response: MammaVisitatielijstResponseDto) => {
        if (response.meldingen.length) {
          this.toastService.warning(response.meldingen)
        }
        this.dialogRef.close({ request, bestand, rapport: response.rapport })
      })
  }
}
