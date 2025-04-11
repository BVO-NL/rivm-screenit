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
import { ClrCommonFormsModule, ClrDatepickerModule, ClrFileInputModule, ClrInputModule } from '@clr/angular'
import { FormControl, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { formatDate } from '@shared/date-utils'
import { valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { bsnValidator } from '@shared/validators/bsn/bsn.validator'
import { ToastService } from '@/toast/service/toast.service'
import { ExtraBeveiligdeOmgevingService } from '@/algemeen/extra-beveiligde-omgeving/services/extra-beveiligde-omgeving/extra-beveiligde-omgeving.service'
import { take } from 'rxjs'

@Component({
  selector: 'app-extra-beveiligde-omgeving-keuze-herstellen-page',
  imports: [ClrCommonFormsModule, ReactiveFormsModule, ClrInputModule, ClrDatepickerModule, ClrFileInputModule],
  templateUrl: './extra-beveiligde-omgeving-keuze-herstellen-page.component.html',
  styleUrl: './extra-beveiligde-omgeving-keuze-herstellen-page.component.scss',
})
export class ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly toastService = inject(ToastService)
  private readonly extraBeveiligdeOmgevingService = inject(ExtraBeveiligdeOmgevingService)

  formGroup = this.formBuilder.group({
    geboortedatum: ['', [Validators.required, valideDatumValidator]],
    bsn: ['', [Validators.required, bsnValidator]],
    bestand: this.formBuilder.control<FileList | null>(null, [Validators.required]),
  })
  maxGeboorteDatum = formatDate(new Date())

  get bsnCtrl(): FormControl {
    return this.formGroup.get('bsn') as FormControl
  }

  get geboortedatumCtrl(): FormControl {
    return this.formGroup.get('geboortedatum') as FormControl
  }

  keuzeHerstellen() {
    if (this.formGroup.invalid) {
      return
    }
    const formValue = this.formGroup.getRawValue()
    this.extraBeveiligdeOmgevingService
      .bezwaarHerstellen(formValue.bsn, formValue.geboortedatum, formValue.bestand![0])
      .pipe(take(1))
      .subscribe(() => {
        this.toastService.success('Keuze gebruik gegevens BRP hersteld')
      })
  }
}
