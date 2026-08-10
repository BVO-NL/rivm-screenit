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
import { Component, effect, forwardRef, input } from '@angular/core'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule, Validators } from '@angular/forms'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'
import { DsRadiobuttonComponent, DsRadiobuttonOption } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-colon-rooster-beperking',
  imports: [ReactiveFormsModule, DsRadiobuttonComponent],
  template: ` <ds-radiobutton [label]="label()" [formControl]="formControl" [items]="items" [columnCount]="2" [columnWidth]="50"></ds-radiobutton>`,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => ColonRoosterBeperkingComponent),
      multi: true,
    },
  ],
})
export class ColonRoosterBeperkingComponent implements ControlValueAccessor {
  label = input.required<string>()
  required = input<boolean>(false)

  formControl: FormControl = new FormControl<string>('ZACHT')
  items: DsRadiobuttonOption<string>[] = [
    {
      label: 'Zacht',
      value: ColonRoosterBeperking.ZACHT,
    },
    {
      label: 'Hard',
      value: ColonRoosterBeperking.HARD,
    },
  ]

  private onChange: ((value: string) => void) | undefined
  private onTouched: (() => void) | undefined

  constructor() {
    this.formControl.valueChanges.pipe(takeUntilDestroyed()).subscribe((res) => {
      if (this.onChange) {
        this.onChange(res)
      }
      if (this.onTouched) {
        this.onTouched()
      }
    })

    effect(() => {
      if (this.required()) {
        this.formControl.addValidators(Validators.required)
      } else {
        this.formControl.clearValidators()
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
