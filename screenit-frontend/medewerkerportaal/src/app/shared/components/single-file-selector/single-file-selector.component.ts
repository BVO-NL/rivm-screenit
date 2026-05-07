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
import { Component, computed, effect, ElementRef, forwardRef, input, Signal, viewChild } from '@angular/core'
import { DsButtonComponent, DsIconComponent } from '@topicus-rgp-ds/web'
import { ControlValueAccessor, FormControl, FormsModule, NG_VALIDATORS, NG_VALUE_ACCESSOR, ReactiveFormsModule, ValidationErrors, Validator, Validators } from '@angular/forms'
import { faClose, faFileUpload } from '@fortawesome/pro-light-svg-icons'
import { extensieValidator } from '@shared/validators/file/file.validator'
import { getMimetypeVanExtensie } from '@shared/utils/file-utils'

@Component({
  selector: 'app-single-file-selector',
  imports: [DsButtonComponent, DsIconComponent, ReactiveFormsModule, FormsModule],
  templateUrl: './single-file-selector.component.html',
  styles: `
    .ds-form-element {
      margin-top: 1rem;
    }
  `,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => SingleFileSelectorComponent),
      multi: true,
    },
    {
      provide: NG_VALIDATORS,
      useExisting: forwardRef(() => SingleFileSelectorComponent),
      multi: true,
    },
  ],
})
export class SingleFileSelectorComponent implements ControlValueAccessor, Validator {
  fileSelector: Signal<ElementRef> = viewChild.required('fileSelector')
  isDisabled = false
  acceptedExtensie = input<string>('')
  acceptedMimetype = computed(() => getMimetypeVanExtensie(this.acceptedExtensie()))
  label = input<string>('Bestand')
  required = input<boolean>(false)
  fileUpload = faFileUpload
  deleteIcon = faClose

  private onChange: ((file: File | null) => void) | undefined
  private onTouched: (() => void) | undefined

  fileCtrl = new FormControl<FileList | null>(null, [Validators.required])
  file: File | null = null

  constructor() {
    effect(() => {
      if (this.acceptedExtensie()) {
        this.fileCtrl.addValidators(extensieValidator([this.acceptedExtensie()]))
      }
      this.fileCtrl.updateValueAndValidity()
    })
  }

  verwerkBestand(event: Event): void {
    const inputElement = event.target as HTMLInputElement | null
    const fileList = inputElement?.files || null
    const geselecteerdBestand = fileList?.item(0) ?? null

    this.file = geselecteerdBestand
    this.fileCtrl.setValue(fileList)
    this.fileCtrl.markAsDirty()
    this.onChange?.(geselecteerdBestand)
    this.onTouched?.()
    this.fileCtrl.updateValueAndValidity()

    if (inputElement) {
      inputElement.value = ''
    }
  }

  openFileSelector(): void {
    if (this.fileSelector().nativeElement) {
      ;(this.fileSelector().nativeElement as HTMLInputElement).click()
    }
  }

  clearFile(): void {
    this.file = null
    this.fileCtrl.setValue(null)
    this.onChange?.(null)
    this.onTouched?.()

    const inputElement = this.fileSelector().nativeElement as HTMLInputElement | null
    if (inputElement) {
      inputElement.value = ''
    }
    this.fileCtrl.updateValueAndValidity()
  }

  writeValue(value: File | null): void {
    this.file = value
    this.fileCtrl.setValue(null)
  }

  registerOnChange(fn: (file: File | null) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled = isDisabled
  }

  validate(): ValidationErrors | null {
    return this.fileCtrl.dirty ? this.fileCtrl.errors : null
  }
}
