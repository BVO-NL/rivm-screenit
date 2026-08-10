/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { Component, computed, forwardRef, inject, input, signal } from '@angular/core'
import { LocatieDto, LocatieStatus } from '../../models/LocatieDto'
import { LocatieService } from '../../services/locatie/locatie.service'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule } from '@angular/forms'
import { DsDropdownComponent } from '@topicus-rgp-ds/web'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { filter } from 'rxjs'

@Component({
  selector: 'app-locatie-selector',
  imports: [ReactiveFormsModule, DsDropdownComponent],
  template: `<ds-dropdown [items]="locaties()" [label]="label()" [formControl]="locatieCtrl" bindValue="value" />`,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => LocatieSelectorComponent),
      multi: true,
    },
  ],
})
export class LocatieSelectorComponent implements ControlValueAccessor {
  private readonly locatieService = inject(LocatieService)

  private alleLocaties = toSignal(this.locatieService.getLocaties(0, 10, LocatieStatus.ACTIEF), { initialValue: [] })
  locaties = computed(() => this.alleLocaties().map((locatie) => ({ value: locatie, label: locatie.naam })))
  locatieCtrl = new FormControl<LocatieDto | null>(null)
  label = input<string>('Locatie')

  private onChange: ((value: LocatieDto | null) => void) | undefined
  private onTouched: (() => void) | undefined
  isDisabled = signal(false)

  constructor() {
    this.locatieCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.locatieCtrl.valid),
      )
      .subscribe((value: LocatieDto | null) => {
        if (this.onChange) {
          this.onChange(value)
        }
      })
  }
  writeValue(obj: LocatieDto): void {
    this.locatieCtrl.patchValue(obj)
  }
  registerOnChange(fn: (value: LocatieDto | null) => void): void {
    this.onChange = fn
  }
  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }
  setDisabledState?(isDisabled: boolean): void {
    this.isDisabled.set(isDisabled)
    if (isDisabled) {
      this.locatieCtrl.disable()
    } else {
      this.locatieCtrl.enable()
    }
  }
}
