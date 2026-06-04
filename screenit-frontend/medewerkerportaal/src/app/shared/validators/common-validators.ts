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
import { AbstractControl, ValidationErrors, ValidatorFn, Validators } from '@angular/forms'

export function trimmedValidator(validator: ValidatorFn): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    if (!control.value || typeof control.value !== 'string') {
      return validator(control)
    }
    const trimmed = control.value.trim()
    const trimmedControl = { ...control, value: trimmed } as AbstractControl
    return validator(trimmedControl)
  }
}

export const positiveIntegerValidator = Validators.pattern('^[0-9]*$')
export const huisnummerValidator = [trimmedValidator(Validators.maxLength(10)), trimmedValidator(Validators.pattern(/^\d*$/))]
export const briefkenmerkValidator = trimmedValidator(Validators.pattern(/^[Kk].+$/))
export const telefoonnummerValidator = Validators.pattern(/^(06|\+316|00316)[- ]?[0-9]{8}$/)
