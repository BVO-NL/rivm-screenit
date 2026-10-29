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
import { Component, computed, forwardRef, inject, input, signal, WritableSignal } from '@angular/core'
import { DsButtonComponent, DsIconComponent, DsTextareaComponent } from '@topicus-rgp-ds/web'
import { faPlus } from '@fortawesome/pro-light-svg-icons'
import { AbstractControl, ControlValueAccessor, FormsModule, NG_VALIDATORS, NG_VALUE_ACCESSOR, ValidationErrors, Validator, Validators } from '@angular/forms'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-notitie-aanmaken-veld',
  imports: [DsButtonComponent, DsIconComponent, DsTextareaComponent, FormsModule],
  templateUrl: './notitie-aanmaken-veld.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => NotitieAanmakenVeldComponent),
      multi: true,
    },
    {
      provide: NG_VALIDATORS,
      useExisting: forwardRef(() => NotitieAanmakenVeldComponent),
      multi: true,
    },
  ],
  styles: `
    :host {
      display: block;
    }
  `,
})
export class NotitieAanmakenVeldComponent implements ControlValueAccessor, Validator {
  protected readonly faPlus = faPlus
  protected veldZichtbaar: WritableSignal<boolean> = signal(false)
  protected isDisabled = signal(false)
  protected value = signal('')
  private readonly strings = inject(StringsService).getStrings().algemeen
  protected readonly toevoegenLabel = this.strings.notities.buttons.notitieToevoegen
  protected readonly placeholder = this.strings.notities.placeholder
  label = input<string>(this.strings.labels.notitie)
  gebruikExterneLabel = input<boolean>(false)
  interneLabel = computed(() => (this.gebruikExterneLabel() ? '' : this.label()))
  externeLabel = computed(() => (this.gebruikExterneLabel() ? this.label() : ''))
  maxLength = input(2048)
  protected readonly foutmeldingen = computed(() => ({ maxlength: this.strings.errors.maxKarakters(this.label(), this.maxLength()) }))

  protected onChange: ((val: string) => void) | null = null
  private onTouched: (() => void) | null = null

  writeValue(obj: string): void {
    this.value.set(obj)
  }

  registerOnChange(fn: (val: string) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled.set(isDisabled)
  }

  onUpdate(value: string) {
    this.onChange!(value)
    this.onTouched!()
  }

  validate(control: AbstractControl): ValidationErrors | null {
    return Validators.maxLength(this.maxLength())(control)
  }

  protected toonVeld() {
    this.veldZichtbaar.set(true)
  }
}
