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
import { Component, forwardRef, inject, Signal } from '@angular/core'

import { ClrComboboxModule, ClrCommonFormsModule } from '@clr/angular'
import { ControlValueAccessor, FormControl, NG_VALIDATORS, NG_VALUE_ACCESSOR, ReactiveFormsModule, ValidationErrors, Validator } from '@angular/forms'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter } from 'rxjs'

@Component({
  selector: 'app-kamer-selector',
  imports: [ClrComboboxModule, ClrCommonFormsModule, ReactiveFormsModule],
  styles: [
    `
      :host {
        display: block;
      }
    `,
  ],
  template: ` <clr-combobox-container class="no-error-icon">
    <label class="clr-required-mark" for="kamer">Kamer</label>
    <clr-combobox [formControl]="kamerCtrl" required id="kamer" data-testid="cb_kamer_selector">
      <clr-options>
        <clr-option [clrValue]="alleKamers">{{ alleKamers.naam }}</clr-option>
        <clr-option [clrValue]="kamer" *clrOptionItems="let kamer of kamers$(); field: 'naam'">{{ kamer.naam }}</clr-option>
      </clr-options>
    </clr-combobox>
    <clr-control-error>De kamer is verplicht</clr-control-error>
  </clr-combobox-container>`,
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
  private roosterService: RoosterService = inject(RoosterService)
  private onChange: ((value: number | undefined) => void) | undefined
  private onTouched: (() => void) | undefined

  kamers$: Signal<ColonKamer[]> = this.roosterService.kamers
  kamerCtrl = new FormControl<ColonKamer | null>(null)
  alleKamers: ColonKamer = { id: 0, naam: 'Alle kamers' }

  constructor() {
    this.kamerCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.onChange != undefined),
      )
      .subscribe((kamer) => this.onChange!(kamer?.id))
  }

  registerOnChange(fn: (value: number | undefined) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  writeValue(value: number): void {
    const kamer = this.kamers$().find((kamer) => kamer.id === value)
    if (kamer) {
      this.kamerCtrl.patchValue(kamer)
    }
  }

  setDisabledState(isDisabled: boolean) {
    if (isDisabled) {
      this.kamerCtrl.disable()
    } else {
      this.kamerCtrl.enable()
    }
  }

  validate(): ValidationErrors | null {
    return this.kamerCtrl.valid ? null : { invalid: true }
  }
}
