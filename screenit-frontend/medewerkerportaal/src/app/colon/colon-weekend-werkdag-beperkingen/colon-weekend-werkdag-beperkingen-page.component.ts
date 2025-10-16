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

import { ClrAlertModule, ClrCheckboxModule, ClrCommonFormsModule, ClrDatepickerModule, ClrIconModule, ClrInputModule, ClrRadioModule } from '@clr/angular'
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'
import { ColonRoosterBeperkingComponent } from '@/colon/components/colon-rooster-beperking/colon-rooster-beperking.component'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonRoosterBeperkingenDto } from '@shared/types/colon/colon-rooster-beperkingen-dto'
import { take } from 'rxjs'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/autorisatie/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { ToastService } from '@shared/toast/service/toast.service'

@Component({
  selector: 'app-colon-weekend-werkdag-beperkingen-page',
  imports: [
    ClrCommonFormsModule,
    ClrInputModule,
    FormsModule,
    ClrDatepickerModule,
    ClrCheckboxModule,
    ReactiveFormsModule,
    ClrRadioModule,
    ColonRoosterBeperkingComponent,
    ClrAlertModule,
    ClrIconModule,
    AutorisatieDirective,
  ],
  templateUrl: './colon-weekend-werkdag-beperkingen-page.component.html',
  styles: [
    `
      .not-greyed-out {
        color: #454545;
      }
      .nacht-beperking {
        display: flex;
        flex-direction: row;
        justify-content: space-between;

        input {
          margin-right: 1rem;
        }
      }
    `,
  ],
})
export class ColonWeekendWerkdagBeperkingenPageComponent {
  private formBuilder: FormBuilder = inject(FormBuilder)
  private roosterService: RoosterService = inject(RoosterService)
  private toastService: ToastService = inject(ToastService)
  private autorisatieService: AutorisatieService = inject(AutorisatieService)

  beperkingForm: FormGroup = this.formBuilder.group({
    nachtBeperkingBegin: ['', Validators.required],
    nachtBeperkingEind: ['', Validators.required],
    nachtBeperkingBeginDisabled: [{ value: '23:59', disabled: true }, Validators.required],
    nachtBeperkingEindDisabled: [{ value: '00:00', disabled: true }, Validators.required],
    nachtBeperkingType: ColonRoosterBeperking.HARD,
    zaterdagBeperkingType: ColonRoosterBeperking.ZACHT,
    zondagBeperkingType: ColonRoosterBeperking.ZACHT,
  })
  constraint: SecurityConstraint = {
    recht: ['COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER'],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }

  constructor() {
    this.roosterService
      .getBeperkingen()
      .pipe(takeUntilDestroyed())
      .subscribe((res: ColonRoosterBeperkingenDto) => {
        if (res.nachtBeperkingType) {
          this.beperkingForm.patchValue(res)
        }
      })

    if (!this.autorisatieService.isToegestaan(this.constraint)) {
      this.beperkingForm.disable()
    }
  }

  save() {
    if (this.beperkingForm.invalid) {
      return
    }

    const beperking = {
      nachtBeperkingBegin: this.beperkingForm.get('nachtBeperkingBegin')?.value,
      nachtBeperkingEind: this.beperkingForm.get('nachtBeperkingEind')?.value,
      nachtBeperkingType: this.beperkingForm.get('nachtBeperkingType')?.value,
      zaterdagBeperkingType: this.beperkingForm.get('zaterdagBeperkingType')?.value,
      zondagBeperkingType: this.beperkingForm.get('zondagBeperkingType')?.value,
    }

    this.roosterService
      .updateBeperkingen(beperking)
      .pipe(take(1))
      .subscribe(() => {
        this.toastService.success('De beperkingen zijn opgeslagen')
      })
  }

  isVeldDisabled: boolean = true
}
