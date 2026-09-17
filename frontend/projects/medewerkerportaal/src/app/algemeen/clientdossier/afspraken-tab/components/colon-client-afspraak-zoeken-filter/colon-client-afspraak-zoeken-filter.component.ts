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
import { Component, inject, output } from '@angular/core'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { VrijSlotZonderKamerFilter } from '@shared/types/colon/dto/vrij-slot-zonder-kamer-filter'
import { DropdownValueModel, DsButtonComponent, DsDatepickerComponent, DsDropdownComponent, DsIconComponent, DsOptionTemplateDirective } from '@topicus-rgp-ds/web'
import { ColonIntakeafspraakAanmakenType, colonIntakeafspraakAanmakenTypeLabels } from '@shared/types/colon/enum/colon-intakeafspraak-aanmaken-type'
import { faSearch } from '@fortawesome/pro-light-svg-icons'
import { ClientService } from '@algemeen/services/client/client.service'
import { ParameterService } from '@algemeen/services/parameter/parameter.service'
import { ParameterKey } from '@shared/types/algemeen/enum/parameter-key'
import { addBusinessDays } from 'date-fns'
import { RoosterService } from '@colon/colon-rooster-page/services/rooster.service'
import { take } from 'rxjs'

@Component({
  selector: 'app-colon-client-afspraak-zoeken-filter',
  imports: [ReactiveFormsModule, DsDropdownComponent, DsDatepickerComponent, DsButtonComponent, DsIconComponent, DsOptionTemplateDirective],
  templateUrl: './colon-client-afspraak-zoeken-filter.component.html',
  styleUrl: './colon-client-afspraak-zoeken-filter.component.scss',
})
export class ColonClientAfspraakZoekenFilterComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientService = inject(ClientService)
  private readonly parameterService = inject(ParameterService)
  private readonly roosterService = inject(RoosterService)

  zoeken = output<VrijSlotZonderKamerFilter>()

  protected readonly afspraakTypeItems: DropdownValueModel<ColonIntakeafspraakAanmakenType>[] = Object.values(ColonIntakeafspraakAanmakenType).map((type) => ({
    label: colonIntakeafspraakAanmakenTypeLabels[type],
    value: type,
  }))

  protected readonly searchIcon = faSearch

  get vanafCtrl(): FormControl {
    return this.zoekenForm.get('vanaf') as FormControl
  }

  zoekenForm = this.formBuilder.group({
    clientId: this.clientService.clientId(),
    type: [ColonIntakeafspraakAanmakenType.STANDAARD, [Validators.required]],
    vanaf: this.formBuilder.control<Date | null>(null, Validators.required),
    totEnMet: this.formBuilder.control<Date | null>(null, Validators.required),
  })

  constructor() {
    this.zetFilters()
  }

  private zetFilters() {
    if (this.zoekenForm.get('type')?.value === ColonIntakeafspraakAanmakenType.STANDAARD) {
      this.roosterService
        .getInstellingen()
        .pipe(take(1))
        .subscribe((instellingen) => {
          const nietWijzigbarePeriode = Number(this.parameterService.getParameterWaarde<number>(ParameterKey.INTAKE_NIET_WIJZIGBAAR, 3))
          const vanaf = addBusinessDays(new Date(), nietWijzigbarePeriode)
          const tot = instellingen.geprognosticeerdeVanafDatum

          this.zoekenForm.get('vanaf')!.setValue(vanaf)
          this.zoekenForm.get('totEnMet')!.setValue(tot)
        })
    }
  }

  verstuurQuery() {
    this.zoeken.emit(this.zoekenForm.value as VrijSlotZonderKamerFilter)
  }
}
