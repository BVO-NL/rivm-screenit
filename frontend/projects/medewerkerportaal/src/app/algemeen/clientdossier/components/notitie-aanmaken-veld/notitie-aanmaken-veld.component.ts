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
import { Component, computed, forwardRef, input, signal, WritableSignal } from '@angular/core'
import { DsButtonComponent, DsIconComponent, DsTextareaComponent } from '@topicus-rgp-ds/web'
import { faPlus } from '@fortawesome/pro-light-svg-icons'
import { ControlValueAccessor, FormsModule, NG_VALUE_ACCESSOR } from '@angular/forms'

@Component({
  selector: 'app-notitie-aanmaken-veld',
  imports: [DsButtonComponent, DsIconComponent, DsTextareaComponent, FormsModule],
  templateUrl: './notitie-aanmaken-veld.component.html',
  styleUrl: './notitie-aanmaken-veld.component.scss',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => NotitieAanmakenVeldComponent),
      multi: true,
    },
  ],
})
export class NotitieAanmakenVeldComponent implements ControlValueAccessor {
  protected readonly faPlus = faPlus
  protected veldZichtbaar: WritableSignal<boolean> = signal(false)
  protected isDisabled = false
  protected value = ''
  label = input<string>('Notitie')
  gebruikExterneLabel = input<boolean>(false)
  interneLabel = computed(() => (this.gebruikExterneLabel() ? '' : this.label()))
  externeLabel = computed(() => (this.gebruikExterneLabel() ? this.label() : ''))

  protected onChange: ((val: string) => void) | null = null
  private onTouched: (() => void) | null = null

  writeValue(obj: string): void {
    this.value = obj
  }

  registerOnChange(fn: (val: string) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled = isDisabled
  }

  onUpdate(value: string) {
    this.onChange!(value)
    this.onTouched!()
  }

  protected toonVeld() {
    this.veldZichtbaar.set(true)
  }
}
