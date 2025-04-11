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
import { AfterViewInit, Component, DestroyRef, forwardRef, inject, OnInit } from '@angular/core'

import { ClrCheckboxModule, ClrComboboxModule, ClrCommonFormsModule, ClrDatepickerModule, ClrInputModule } from '@clr/angular'
import {
  ControlContainer,
  ControlValueAccessor,
  FormArray,
  FormBuilder,
  FormControl,
  FormGroup,
  FormsModule,
  NG_VALIDATORS,
  NG_VALUE_ACCESSOR,
  ReactiveFormsModule,
  ValidationErrors,
  Validator,
  Validators,
} from '@angular/forms'
import { ColonHerhaling } from '@shared/types/colon/colon-herhaling'
import { COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN, WEEK_DAGEN } from '@shared/constants'
import { SelectOption } from '@shared/types/select-option'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { formatDate, formatNLDate, parseDate } from '@shared/date-utils'
import { add, addWeeks, isValid } from 'date-fns'
import { distinctUntilChanged, filter, startWith } from 'rxjs'
import { ColonHerhalingsfrequentie } from '@shared/types/colon/colon-herhaling-frequentie'
import { WeekdagenSelectorComponent } from '@/colon/colon-rooster-page/components/weekdagen-selector/weekdagen-selector.component'
import { dagenSelectieCheckboxValidator } from '@shared/validators/dagen-selectie/dagen-selectie.validator'
import { createMaximumDatumValidator, createMinimumDatumValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'

@Component({
  selector: 'app-bulk-aanmaken',
  imports: [ClrComboboxModule, ClrCommonFormsModule, FormsModule, ReactiveFormsModule, ClrCheckboxModule, ClrDatepickerModule, ClrInputModule, WeekdagenSelectorComponent],
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
export class BulkAanmakenFormComponent implements ControlValueAccessor, Validator, OnInit, AfterViewInit {
  private formBuilder: FormBuilder = inject(FormBuilder)
  private destroyRef: DestroyRef = inject(DestroyRef)
  parentForm = inject(ControlContainer, { optional: true, host: true, skipSelf: true })

  private onChange: ((value: ColonHerhaling) => void) | undefined
  private onTouched: (() => void) | undefined

  get geselecteerdeFrequentie(): ColonHerhalingsfrequentie | undefined {
    return this.herhalingsFrequentieCtrl.value?.value
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

  herhalingsfrequentie = ColonHerhalingsfrequentie
  dagen: SelectOption<number>[] = WEEK_DAGEN
  maxDatum = formatDate(add(new Date(), { months: COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN }))
  minDatum = formatDate(add(new Date(), { days: 1 }))
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

  herhalingForm: FormGroup = this.formBuilder.group({
    frequentie: this.herhalingOpties[0],
    dagen: [[], dagenSelectieCheckboxValidator],
    alleenWerkdagen: true,
    eindDatum: [null, { updateOn: 'blur' }],
  })

  constructor() {
    this.herhalingsFrequentieCtrl.valueChanges.pipe(takeUntilDestroyed(), distinctUntilChanged()).subscribe((option: SelectOption<string>) => {
      if (option == null) {
        return
      }

      this.herhalingForm.reset(
        {
          alleenWerkdagen: true,
          frequentie: option,
          eindDatum: this.eindDatumVanTijdslot,
          dagen: [],
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

  ngOnInit(): void {
    this.handleAfspraakslotDatumChange()
  }

  ngAfterViewInit(): void {
    this.handleChange()
  }

  private handleAfspraakslotDatumChange(): void {
    this.tijdslotDatumCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(this.destroyRef),
        distinctUntilChanged(),
        filter(() => this.tijdslotDatumCtrl.value != null && isValid(parseDate(this.tijdslotDatumCtrl.value))),
        startWith(this.tijdslotDatumCtrl.value),
      )
      .subscribe(() => {
        const afspraakslotDatum = parseDate(this.tijdslotDatumCtrl.value)
        this.minDatum = formatDate(add(afspraakslotDatum, { days: 1 }))
        this.maxDatum = formatDate(add(afspraakslotDatum, { months: COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN }))

        if (!this.eindDatumCtrl.dirty) {
          this.eindDatumCtrl.setValue(this.eindDatumVanTijdslot)
        }

        this.updateValidaties()
      })
  }

  private get eindDatumVanTijdslot(): string | null {
    return isValid(parseDate(this.tijdslotDatumCtrl.value)) ? formatNLDate(addWeeks(parseDate(this.tijdslotDatumCtrl.value), 2)) : null
  }

  private setValidators() {
    const option: SelectOption<string> = this.herhalingsFrequentieCtrl.value
    switch (option.value) {
      case ColonHerhalingsfrequentie.GEEN_HERHALING:
        this.eindDatumCtrl.clearValidators()
        this.dagenCtrl.clearValidators()
        break
      case ColonHerhalingsfrequentie.DAGELIJKS:
        this.eindDatumCtrl.setValidators([
          Validators.required,
          valideDatumValidator,
          createMinimumDatumValidator(this.tijdslotDatumCtrl, 1),
          createMaximumDatumValidator(this.tijdslotDatumCtrl, COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN),
        ])
        this.dagenCtrl.clearValidators()
        break
      case ColonHerhalingsfrequentie.WEKELIJKS:
      case ColonHerhalingsfrequentie.TWEE_WEKELIJKS:
        this.eindDatumCtrl.setValidators([
          Validators.required,
          valideDatumValidator,
          createMinimumDatumValidator(this.tijdslotDatumCtrl, 1),
          createMaximumDatumValidator(this.tijdslotDatumCtrl, COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN),
        ])
        this.dagenCtrl.setValidators(dagenSelectieCheckboxValidator)
    }

    this.updateValidaties()
  }

  private updateValidaties() {
    setTimeout(() => {
      Object.keys(this.herhalingForm.controls).forEach((field) => {
        const control = this.herhalingForm.get(field)
        control?.updateValueAndValidity()
      })
    }, 0)
  }

  private handleChange() {
    if (this.onChange === undefined) {
      return
    }
    const rawValue = this.herhalingForm.value
    const herhaling = {
      ...rawValue,
      frequentie: rawValue.frequentie?.value,
      weekdag: rawValue.weekdag?.value,
      eindDatum: rawValue.eindDatum && isValid(parseDate(rawValue.eindDatum)) ? formatDate(parseDate(rawValue.eindDatum)) : null,
      dagen: rawValue.dagen,
    }

    this.onChange(herhaling)
  }

  registerOnChange(fn: (value: ColonHerhaling) => void): void {
    this.onChange = fn
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

  writeValue(obj: ColonHerhaling): void {
    this.herhalingForm.patchValue(obj, { emitEvent: false })
  }

  validate(): ValidationErrors | null {
    return this.herhalingForm.valid ? null : { invalid: true }
  }
}
