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
import { afterNextRender, Component, DestroyRef, forwardRef, inject, Injector, OnInit } from '@angular/core'
import {
  ControlContainer,
  ControlValueAccessor,
  FormArray,
  FormBuilder,
  FormControl,
  FormGroup,
  NG_VALIDATORS,
  NG_VALUE_ACCESSOR,
  ReactiveFormsModule,
  ValidationErrors,
  Validator,
  Validators,
} from '@angular/forms'
import { ColonHerhaling } from '@shared/types/colon/colon-herhaling'
import { COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN, TDS_DAGEN } from '@shared/constants'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { distinctUntilChanged, filter, startWith } from 'rxjs'
import { ColonHerhalingsfrequentie } from '@shared/types/colon/colon-herhaling-frequentie'
import { dagenSelectieCheckboxValidator } from '@shared/validators/dagen-selectie/dagen-selectie.validator'
import { DsCheckboxComponent, DsDatepickerComponent, DsDropdownComponent, DsWeekdaySelectorComponent } from '@topicus-rgp-ds/web'
import { WeekdagOptie } from '@shared/types/weekdag-optie'
import { addDays, addMonths, addWeeks, isValid } from 'date-fns'
import { formatDate } from '@shared/utils/date-utils'

@Component({
  selector: 'app-bulk-aanmaken',
  imports: [ReactiveFormsModule, DsCheckboxComponent, DsDatepickerComponent, DsDropdownComponent, DsWeekdaySelectorComponent],
  templateUrl: './bulk-aanmaken-form.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => BulkAanmakenFormComponent),
      multi: true,
    },
    {
      provide: NG_VALIDATORS,
      useExisting: forwardRef(() => BulkAanmakenFormComponent),
      multi: true,
    },
  ],
})
export class BulkAanmakenFormComponent implements ControlValueAccessor, Validator, OnInit {
  parentForm = inject(ControlContainer, { optional: true, host: true, skipSelf: true })
  herhalingsfrequentie = ColonHerhalingsfrequentie
  dagen = TDS_DAGEN
  maxDatum: string = addMonths(new Date(), COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN).toISOString()
  minDatum: string = addDays(new Date(), 1).toISOString()
  herhalingOpties = [
    {
      label: 'Niet herhalen',
      value: ColonHerhalingsfrequentie.GEEN_HERHALING,
    },
    {
      label: 'Dagelijks',
      value: ColonHerhalingsfrequentie.DAGELIJKS,
    },
    {
      label: 'Wekelijks',
      value: ColonHerhalingsfrequentie.WEKELIJKS,
    },
    {
      label: '2-wekelijks',
      value: ColonHerhalingsfrequentie.TWEE_WEKELIJKS,
    },
  ]
  private formBuilder: FormBuilder = inject(FormBuilder)
  herhalingForm: FormGroup = this.formBuilder.group({
    frequentie: [this.herhalingOpties[0].value, Validators.required],
    dagen: [TDS_DAGEN, Validators.required],
    alleenWerkdagen: true,
    eindDatum: [null, Validators.required],
  })
  private destroyRef: DestroyRef = inject(DestroyRef)
  private injector = inject(Injector)
  private onChange: ((value: ColonHerhaling) => void) | undefined
  private onTouched: (() => void) | undefined

  constructor() {
    this.herhalingsFrequentieCtrl.valueChanges.pipe(takeUntilDestroyed(), distinctUntilChanged()).subscribe((option: string) => {
      if (option == null) {
        return
      }

      this.herhalingForm.reset(
        {
          alleenWerkdagen: true,
          frequentie: option,
          eindDatum: this.eindDatumVanTijdslot,
          dagen: TDS_DAGEN,
        },
        { emitEvent: false },
      )

      this.setValidators()
    })

    this.herhalingForm.valueChanges.pipe(takeUntilDestroyed(), distinctUntilChanged()).subscribe(() => {
      if (this.onChange) {
        this.handleChange()
      }
      if (this.onTouched) {
        this.onTouched()
      }
    })
  }

  get geselecteerdeFrequentie(): ColonHerhalingsfrequentie | undefined {
    return this.herhalingsFrequentieCtrl.value
  }

  get herhalingsFrequentieCtrl(): FormControl {
    return this.herhalingForm.get('frequentie') as FormControl
  }

  get eindDatumCtrl(): FormControl {
    return this.herhalingForm.get('eindDatum') as FormControl
  }

  get dagenCtrl(): FormArray {
    return this.herhalingForm.get('dagen') as FormArray
  }

  get tijdslotDatumCtrl(): FormControl {
    return this.parentForm?.control?.get('datum') as FormControl
  }

  private get eindDatumVanTijdslot(): Date | null {
    return isValid(this.tijdslotDatumCtrl.value) ? addWeeks(this.tijdslotDatumCtrl.value, 2) : null
  }

  ngOnInit(): void {
    this.handleAfspraakslotDatumChange()
  }

  registerOnChange(fn: (value: ColonHerhaling) => void): void {
    this.onChange = fn
    this.handleChange()
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    if (isDisabled) {
      this.herhalingForm.disable()
    } else {
      this.herhalingForm.enable()
    }
  }

  writeValue(obj: ColonHerhaling | null): void {
    if (obj == null) {
      this.herhalingForm.reset(
        {
          frequentie: this.herhalingOpties[0].value,
          dagen: [],
          alleenWerkdagen: true,
          eindDatum: null,
        },
        { emitEvent: false },
      )
    } else {
      this.herhalingForm.patchValue(obj, { emitEvent: false })
    }
  }

  validate(): ValidationErrors | null {
    return this.herhalingForm.valid ? null : { invalid: true }
  }

  private handleAfspraakslotDatumChange(): void {
    this.tijdslotDatumCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(this.destroyRef),
        distinctUntilChanged(),
        filter(() => this.tijdslotDatumCtrl.value != null && isValid(this.tijdslotDatumCtrl.value)),
        startWith(this.tijdslotDatumCtrl.value),
      )
      .subscribe((afspraakslotDatum: Date) => {
        this.minDatum = addDays(afspraakslotDatum, 1).toISOString()
        this.maxDatum = addMonths(afspraakslotDatum, COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN).toISOString()

        if (!this.eindDatumCtrl.dirty) {
          this.eindDatumCtrl.setValue(this.eindDatumVanTijdslot)
        }

        this.updateValidaties()
      })
  }

  private setValidators() {
    switch (this.herhalingsFrequentieCtrl.value) {
      case ColonHerhalingsfrequentie.GEEN_HERHALING:
        this.eindDatumCtrl.clearValidators()
        this.dagenCtrl.clearValidators()
        break
      case ColonHerhalingsfrequentie.DAGELIJKS:
        this.eindDatumCtrl.setValidators([Validators.required])
        this.dagenCtrl.clearValidators()
        break
      case ColonHerhalingsfrequentie.WEKELIJKS:
      case ColonHerhalingsfrequentie.TWEE_WEKELIJKS:
        this.eindDatumCtrl.setValidators([Validators.required])
        this.dagenCtrl.setValidators(dagenSelectieCheckboxValidator)
    }

    this.updateValidaties()
  }

  private updateValidaties() {
    afterNextRender(
      () => {
        Object.keys(this.herhalingForm.controls).forEach((field) => {
          const control = this.herhalingForm.get(field)
          control?.updateValueAndValidity({ emitEvent: false })
        })
      },
      { injector: this.injector },
    )
  }

  private handleChange() {
    const rawValue = this.herhalingForm.value
    const herhaling = {
      ...rawValue,
      eindDatum: rawValue.eindDatum != null ? formatDate(rawValue.eindDatum) : null,
      dagen: rawValue.dagen.filter((dag: WeekdagOptie) => dag.selected).map((dag: WeekdagOptie) => dag.value),
    }

    this.onChange!(herhaling)
  }
}
