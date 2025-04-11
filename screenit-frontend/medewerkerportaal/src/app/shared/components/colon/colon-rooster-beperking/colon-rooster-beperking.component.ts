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
import { Component, forwardRef, Input } from '@angular/core'

import { ClrCommonFormsModule, ClrRadioModule } from '@clr/angular'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule } from '@angular/forms'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'

@Component({
  selector: 'app-colon-rooster-beperking',
  imports: [ClrCommonFormsModule, ClrRadioModule, ReactiveFormsModule],
  templateUrl: './colon-rooster-beperking.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => ColonRoosterBeperkingComponent),
      multi: true,
    },
  ],
})
export class ColonRoosterBeperkingComponent implements ControlValueAccessor {
  private onChange: ((value: string) => void) | undefined
  private onTouched: (() => void) | undefined

  @Input({ required: true }) label = 'label'
  @Input() required = false

  formControl: FormControl = new FormControl<string>('ZACHT')
  zachtId = Math.random().toString()
  hardId = Math.random().toString()
  colonRoosterBeperking = ColonRoosterBeperking

  constructor() {
    this.formControl.valueChanges.pipe(takeUntilDestroyed()).subscribe((res) => {
      if (this.onChange) {
        this.onChange(res)
      }
      if (this.onTouched) {
        this.onTouched()
      }
    })
  }

  registerOnChange(fn: (value: string) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    if (isDisabled) {
      this.formControl.disable()
    } else {
      this.formControl.enable()
    }
  }

  writeValue(obj: ColonRoosterBeperking): void {
    this.formControl.setValue(obj)
  }
}
