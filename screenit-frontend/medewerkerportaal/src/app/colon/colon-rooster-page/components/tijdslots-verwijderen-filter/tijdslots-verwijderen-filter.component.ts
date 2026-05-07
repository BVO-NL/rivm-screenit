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
import { KamerSelectorComponent } from '@/colon/colon-rooster-page/components/kamer-selector/kamer-selector.component'
import { FormControl, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { createStartEindTijdValidator } from '@shared/validators/datum/datum.validator'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonTijdslotFilter } from '@shared/types/colon/colon-tijdslot-filter'
import { TDS_DAGEN } from '@shared/constants'
import { NotificationService } from '@shared/services/notification/notification.service'
import { DsButtonComponent, DsCheckboxComponent, DsDatepickerComponent, DsWeekdaySelectorComponent } from '@topicus-rgp-ds/web'
import { WeekdagOptie } from '@shared/types/weekdag-optie'
import { TimepickerComponent } from '@shared/components/timepicker/timepicker.component'
import { filter } from 'rxjs'

@Component({
  selector: 'app-tijdslots-verwijderen-filter',
  imports: [KamerSelectorComponent, ReactiveFormsModule, DsCheckboxComponent, DsButtonComponent, DsWeekdaySelectorComponent, TimepickerComponent, DsDatepickerComponent],
  templateUrl: './tijdslots-verwijderen-filter.component.html',
  styleUrl: './tijdslots-verwijderen-filter.component.scss',
})
export class TijdslotsVerwijderenFilterComponent {
  private readonly formBuilder: NonNullableFormBuilder = inject(NonNullableFormBuilder)
  private readonly notificationService: NotificationService = inject(NotificationService)
  filter = output<ColonTijdslotFilter>()

  minDatum = new Date().toISOString()
  filterForm = this.formBuilder.group(
    {
      startDatum: this.formBuilder.control<Date | null>(null, [Validators.required]),
      eindDatum: this.formBuilder.control<Date | null>(null, [Validators.required]),
      startTijd: ['', [Validators.required]],
      eindTijd: ['', [Validators.required]],
      kamerId: this.formBuilder.control<number | undefined>(undefined, [Validators.required]),
      dagen: [TDS_DAGEN.map((dag: WeekdagOptie) => ({ ...dag, selected: true })), [Validators.required]],
      heleDag: false,
      alleKamers: false,
    },
    {
      validators: [createStartEindTijdValidator('startTijd', 'eindTijd')],
    },
  )

  get startDatumCtrl(): FormControl {
    return this.filterForm.get('startDatum') as FormControl
  }

  get eindDatumCtrl(): FormControl {
    return this.filterForm.get('eindDatum') as FormControl
  }

  get startTijdCtrl(): FormControl {
    return this.filterForm.get('startTijd') as FormControl
  }

  get eindTijdCtrl(): FormControl {
    return this.filterForm.get('eindTijd') as FormControl
  }

  get heleDagCtrl(): FormControl {
    return this.filterForm.get('heleDag') as FormControl
  }

  get kamerCtrl(): FormControl {
    return this.filterForm.get('kamerId') as FormControl
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

    this.filterForm.statusChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.filterForm.hasError('startEindTijd')),
      )
      .subscribe(() => this.notificationService.error(this.filterForm.getError('startEindTijd')))
  }

  doFilter() {
    if (this.filterForm.invalid) {
      return
    }

    const filterValue = this.filterForm.getRawValue()
    const kamerId = this.kamerCtrl.value === 0 ? undefined : this.kamerCtrl.value
    const alleKamers = this.kamerCtrl.value === 0
    this.filter.emit({
      alleKamers: alleKamers,
      kamerId: kamerId,
      startTijd: filterValue.startTijd,
      eindTijd: filterValue.eindTijd,
      heleDag: filterValue.heleDag,
      dagen: this.filterForm
        .getRawValue()
        .dagen.filter((dag: WeekdagOptie) => dag.selected)
        .map((dag: WeekdagOptie) => dag.value),
      startDatum: this.startDatumCtrl.value,
      eindDatum: this.eindDatumCtrl.value,
    })
  }
}
