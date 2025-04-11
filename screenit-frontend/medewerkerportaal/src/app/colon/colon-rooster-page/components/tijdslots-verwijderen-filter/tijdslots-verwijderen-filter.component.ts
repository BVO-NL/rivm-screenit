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
import { Component, EventEmitter, inject, Output } from '@angular/core'

import { ClrCheckboxModule, ClrCommonFormsModule, ClrDatepickerModule, ClrInputModule } from '@clr/angular'
import { KamerSelectorComponent } from '@/colon/colon-rooster-page/components/kamer-selector/kamer-selector.component'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { WeekdagenSelectorComponent } from '@/colon/colon-rooster-page/components/weekdagen-selector/weekdagen-selector.component'
import { formatDate } from '@shared/date-utils'
import { createStartEindDatumValidator, createStartEindTijdValidator, datumInVerledenValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonTijdslotFilter } from '@shared/types/colon/colon-tijdslot-filter'
import { dagenSelectieSelectValidator } from '@shared/validators/dagen-selectie/dagen-selectie.validator'
import { WEEK_DAGEN } from '@shared/constants'
import { ToastService } from '@/toast/service/toast.service'

@Component({
  selector: 'app-tijdslots-verwijderen-filter',
  imports: [ClrCheckboxModule, ClrCommonFormsModule, ClrDatepickerModule, ClrInputModule, KamerSelectorComponent, ReactiveFormsModule, WeekdagenSelectorComponent],
  templateUrl: './tijdslots-verwijderen-filter.component.html',
})
export class TijdslotsVerwijderenFilterComponent {
  private formBuilder: FormBuilder = inject(FormBuilder)
  @Output() filter: EventEmitter<ColonTijdslotFilter> = new EventEmitter<ColonTijdslotFilter>()
  private toastService: ToastService = inject(ToastService)

  minDatum = formatDate(new Date())
  filterForm = this.formBuilder.group(
    {
      startDatum: ['', { validators: [Validators.required, datumInVerledenValidator, valideDatumValidator], updateOn: 'blur' }],
      eindDatum: ['', { validators: [Validators.required, datumInVerledenValidator, valideDatumValidator], updateOn: 'blur' }],
      startTijd: ['', { validators: [Validators.required], updateOn: 'blur' }],
      eindTijd: ['', { validators: [Validators.required], updateOn: 'blur' }],
      kamerId: [undefined, { validators: [Validators.required] }],
      dagen: this.formBuilder.control<number[]>(
        WEEK_DAGEN.map((dag) => dag.value),
        [dagenSelectieSelectValidator, Validators.required],
      ),
      heleDag: false,
    },
    {
      validators: [
        createStartEindTijdValidator('startTijd', 'eindTijd'),
        createStartEindDatumValidator('startDatum', 'eindDatum', 'De Vanaf datum moet voor de Tot en met datum liggen'),
      ],
    },
  )

  get startTijdCtrl(): FormControl {
    return this.filterForm.get('startTijd') as FormControl
  }

  get eindTijdCtrl(): FormControl {
    return this.filterForm.get('eindTijd') as FormControl
  }

  get startDatumCtrl(): FormControl {
    return this.filterForm.get('startDatum') as FormControl
  }

  get eindDatumCtrl(): FormControl {
    return this.filterForm.get('eindDatum') as FormControl
  }

  get heleDagCtrl(): FormControl {
    return this.filterForm.get('heleDag') as FormControl
  }

  constructor() {
    if (this.heleDagCtrl.value) {
      this.startTijdCtrl.disable()
      this.eindTijdCtrl.disable()
    }

    this.setFormChangeHandlers()
  }

  private setFormChangeHandlers() {
    this.heleDagCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((enabled: boolean) => {
      if (enabled) {
        this.startTijdCtrl.setValue('00:00', { emitEvent: true })
        this.eindTijdCtrl.setValue('23:59', { emitEvent: true })
        this.startTijdCtrl.disable()
        this.eindTijdCtrl.disable()
      } else {
        this.startTijdCtrl.reset(null, { emitEvent: true })
        this.eindTijdCtrl.reset(null, { emitEvent: true })
        this.startTijdCtrl.enable()
        this.eindTijdCtrl.enable()
      }
    })

    this.filterForm.statusChanges.pipe(takeUntilDestroyed()).subscribe(() => {
      this.toastService.clear()
      if (this.filterForm.hasError('startEindTijd')) {
        this.toastService.error(this.filterForm.getError('startEindTijd'))
      }
      if (this.filterForm.hasError('startEindDatum')) {
        this.toastService.error(this.filterForm.getError('startEindDatum'))
      }
    })
  }

  doFilter() {
    if (this.filterForm.invalid) {
      return
    }

    this.filter.emit(this.filterForm.getRawValue() as ColonTijdslotFilter)
  }
}
