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
import { AbstractControl, ValidationErrors, ValidatorFn, Validators } from '@angular/forms'

export class CustomValidators {
  static ibanValidator: ValidatorFn = Validators.pattern(/^([A-Z]{2}[ -]?[0-9]{2})(?=(?:[ -]?[A-Z0-9]){9,30}$)((?:[ -]?[A-Z0-9]{3,5}){2,7})([ -]?[A-Z0-9]{1,3})?$/)
  static numberValidator: ValidatorFn = Validators.pattern(/^[0-9]\d*$/)
  static maakControleValidator = (andereControl: AbstractControl): ValidatorFn => {
    return (control: AbstractControl): ValidationErrors | null => {
      const value1 = control.value
      const value2 = andereControl.value

      if (value1 != null && value2 != null && value1 !== value2) {
        return { waardenOngelijk: 'Dit veld moet overeenkomen met het vorige veld.' }
      }
      return null
    }
  }
  static telefoonnummerValidator: ValidatorFn = Validators.pattern(/^(?:\+31|0031|0)(6\d{8}|[1-57-9]\d{7})$/)
  static postcodeValidator: ValidatorFn = Validators.pattern(/^[0-9]{4}[a-zA-Z]{2}/)
  static wachtwoordSterkteValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
    const invoer = control.value
    if (!invoer) {
      return null
    }

    if (invoer.length < 12) {
      return { wachtwoord: 'Het wachtwoord moet minstens 12 tekens bevatten ' }
    }
    if (invoer.length > 255) {
      return { wachtwoord: 'Het wachtwoord mag hoogstens 255 tekens bevatten ' }
    }
    if (!/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[^A-Za-z0-9])/.test(invoer)) {
      return { wachtwoord: 'Het wachtwoord moet minimaal 1 hoofdletter, 1 kleine letter, 1 nummer en 1 speciaal teken bevatten ' }
    }

    return null
  }
}
