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
import { Component, computed, effect, forwardRef, inject, input, Signal } from '@angular/core'
import { ControlValueAccessor, FormControl, NG_VALIDATORS, NG_VALUE_ACCESSOR, ReactiveFormsModule, ValidationErrors, Validator, Validators } from '@angular/forms'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter } from 'rxjs'
import { DsDropdownComponent } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-kamer-selector',
  imports: [ReactiveFormsModule, DsDropdownComponent],
  styles: [
    `
      :host {
        display: block;
      }
    `,
  ],
  template: ` <ds-dropdown
    label="Kamer"
    [items]="kamerOpties()"
    [formControl]="kamerCtrl"
    data-testid="cb_kamer_selector"
    bindValue="id"
    bindLabel="naam"
    [clearable]="!required()"
    [required]="required()"
    [errorMessage]="{ required: 'De kamer is verplicht' }"
  />`,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => KamerSelectorComponent),
      multi: true,
    },
    {
      provide: NG_VALIDATORS,
      useExisting: forwardRef(() => KamerSelectorComponent),
      multi: true,
    },
  ],
})
export class KamerSelectorComponent implements ControlValueAccessor, Validator {
  private readonly roosterService: RoosterService = inject(RoosterService)
  alleKamers: ColonKamer = { id: 0, naam: 'Alle kamers' }
  kamerCtrl = new FormControl<number | null>(null)

  required = input<boolean>(false)
  inclusiefAlleKamers = input<boolean>(false)

  kamerOpties: Signal<ColonKamer[]> = computed(() => (this.inclusiefAlleKamers() ? [this.alleKamers, ...this.roosterService.kamers()] : this.roosterService.kamers()))

  private onChange: ((value: number | null) => void) | undefined
  private onTouched: (() => void) | undefined

  constructor() {
    this.kamerCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.onChange != undefined),
      )
      .subscribe((kamerId) => this.onChange!(kamerId))

    effect(() => {
      if (this.required()) {
        this.kamerCtrl.addValidators(Validators.required)
      } else {
        this.kamerCtrl.clearValidators()
      }
    })
  }

  registerOnChange(fn: (value: number | null) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  writeValue(value: number): void {
    this.kamerCtrl.patchValue(value)
  }

  setDisabledState(isDisabled: boolean) {
    if (isDisabled) {
      this.kamerCtrl.disable()
    } else {
      this.kamerCtrl.enable()
    }
  }

  validate(): ValidationErrors | null {
    return this.kamerCtrl.errors
  }
}
