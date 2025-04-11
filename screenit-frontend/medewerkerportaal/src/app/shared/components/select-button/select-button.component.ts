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

import { ClrDropdownModule } from '@clr/angular'
import { ControlValueAccessor, NG_VALUE_ACCESSOR } from '@angular/forms'
import { SelectOption } from '@shared/types/select-option'

@Component({
  selector: 'app-select-button',
  imports: [ClrDropdownModule],
  templateUrl: './select-button.component.html',
  styles: [
    `
      .dropdown-toggle {
        justify-content: flex-start;
      }
    `,
  ],
  providers: [{ provide: NG_VALUE_ACCESSOR, useExisting: forwardRef(() => SelectButtonComponent), multi: true }],
})
export class SelectButtonComponent<T> implements ControlValueAccessor {
  @Input() closeOnItemClick = false
  @Input() options: SelectOption<T>[] = []
  @Input() position: 'bottom-left' | 'bottom-right' | 'top-left' | 'top-right' = 'bottom-left'
  onChange: ((value: T) => void) | undefined
  onTouch: ((value: T) => void) | undefined
  disabled = false
  value: T | undefined
  color: 'primary' | 'secondary' | 'default' = 'default'

  get selectedRange(): string {
    return this.options.find((option: SelectOption<T>) => option.value === this.value)?.label ?? 'Onbekend'
  }

  registerOnChange(fn: (value: T) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: (value: T) => void): void {
    this.onTouch = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.disabled = isDisabled
  }

  updateValue(value: T) {
    this.value = value
    if (this.onChange) {
      this.onChange(value)
    }
  }

  writeValue(obj: T): void {
    this.value = obj
  }
}
