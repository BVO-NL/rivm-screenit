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
import { FormControl, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { formatDate, formatNLDate } from '@shared/utils/date-utils'
import { bsnValidator } from '@shared/validators/bsn/bsn.validator'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ExtraBeveiligdeOmgevingService } from '@/algemeen/extra-beveiligde-omgeving/services/extra-beveiligde-omgeving/extra-beveiligde-omgeving.service'
import { take } from 'rxjs'
import { DsButtonComponent, DsDatepickerComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { extensieValidator } from '@shared/validators/file/file.validator'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { PageComponent } from '@shared/components/page/page.component'

@Component({
  selector: 'app-extra-beveiligde-omgeving-keuze-herstellen-page',
  imports: [ReactiveFormsModule, DsDatepickerComponent, DsInputComponent, DsButtonComponent, SingleFileSelectorComponent, PageComponent],
  templateUrl: './extra-beveiligde-omgeving-keuze-herstellen-page.component.html',
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
export class ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly notificationService = inject(NotificationService)
  private readonly extraBeveiligdeOmgevingService = inject(ExtraBeveiligdeOmgevingService)

  formGroup = this.formBuilder.group({
    geboortedatum: this.formBuilder.control<Date | null>(null, [Validators.required]),
    bsn: ['', [Validators.required, bsnValidator]],
    bestand: this.formBuilder.control<File | null>(null, [Validators.required, extensieValidator(['pdf'])]),
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
      .bezwaarHerstellen(formValue.bsn, formatNLDate(formValue.geboortedatum!), formValue.bestand!)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success('Keuze gebruik gegevens BRP hersteld')
      })
  }
}
