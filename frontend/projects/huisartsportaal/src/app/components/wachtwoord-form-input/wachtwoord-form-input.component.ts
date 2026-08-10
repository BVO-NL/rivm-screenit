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
import { afterNextRender, Component, effect, forwardRef, inject, Injector, input, signal, WritableSignal } from '@angular/core'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, NgControl, ReactiveFormsModule, ValidationErrors, Validator, ValidatorFn } from '@angular/forms'
import { CallToActionDirective, DsButtonComponent, DsIconComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { faEye } from '@fortawesome/pro-solid-svg-icons'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter } from 'rxjs'

@Component({
  selector: 'app-wachtwoord-form-input',
  imports: [DsInputComponent, DsIconComponent, CallToActionDirective, DsButtonComponent, ReactiveFormsModule],
  templateUrl: './wachtwoord-form-input.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => WachtwoordFormInputComponent),
      multi: true,
    },
  ],
})
export class WachtwoordFormInputComponent implements ControlValueAccessor, Validator {
  value: string | null = null
  protected readonly eyeIcon = faEye

  private onChange: ((value: string | null) => void) | undefined
  private onTouched: (() => void) | undefined
  private readonly injector = inject(Injector)

  isDisabled = signal(false)
  inputType: WritableSignal<'password' | 'text'> = signal('password')
  required = input<boolean>(false)
  label = input<string>('Wachtwoord')
  errorMessage = input<string | ValidationErrors>('')
  validators = input<ValidatorFn[]>([])
  passwordCtrl = new FormControl('')

  constructor() {
    effect(() => {
      if (this.validators().length > 0) {
        this.passwordCtrl.addValidators(this.validators())
      }

      if (this.isDisabled()) {
        this.passwordCtrl.disable({ emitEvent: false })
      } else {
        this.passwordCtrl.enable({ emitEvent: false })
      }
    })

    this.passwordCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.passwordCtrl.valid),
      )
      .subscribe((value: string | null) => this.onInputChange(value))

    afterNextRender(() => {
      const ngControl = this.injector.get(NgControl, null, { self: true })
      const buitensteControl = ngControl?.control
      if (!buitensteControl) {
        return
      }

      buitensteControl.markAsTouched = () => {
        this.passwordCtrl.markAsTouched({ onlySelf: true })
      }
    })
  }

  verbergWachtwoord() {
    this.inputType.set('password')
  }

  toonWachtwoord() {
    this.inputType.set('text')
  }

  writeValue(obj: string): void {
    this.value = obj
    this.passwordCtrl.setValue(obj, { emitEvent: false })
  }

  registerOnChange(fn: (value: string | null) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled.set(isDisabled)
  }

  onInputChange(value: string | null) {
    if (this.onChange) {
      this.onChange(value)
    }
  }

  onInputBlur() {
    if (this.onTouched) {
      this.onTouched()
    }
  }

  validate(): ValidationErrors | null {
    return this.passwordCtrl.errors
  }
}
