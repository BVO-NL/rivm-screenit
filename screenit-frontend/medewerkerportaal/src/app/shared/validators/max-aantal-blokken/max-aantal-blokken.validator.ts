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
import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms'
import { parse } from 'date-fns'
import { ToastService } from '@/toast/service/toast.service'

export const createMaxAantalBlokkenValidator = (duurAfspraakInMinuten: number, toastService?: ToastService): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty || control.get('id')?.value != null) {
      return null
    }

    const aantalBlokken = control.get('aantalBlokken')?.value
    const startTime = control.get('vanaf')?.value
    const startDate = parse(startTime, 'HH:mm', new Date())
    const midnight = new Date().setHours(24, 0, 0, 0)
    const minutesTillMidnight = (midnight - startDate.getTime()) / 6e4

    let validationError: ValidationErrors | null = null
    let validatieBericht = `Aantal ingevulde afspraakslots is te groot. Vanwege de huidige starttijd is het maximum toegestane aantal afspraakslots:`
    if (minutesTillMidnight) {
      const maxAantalBlokken = Math.floor(minutesTillMidnight / duurAfspraakInMinuten)
      if (maxAantalBlokken < aantalBlokken) {
        validatieBericht = `${validatieBericht} ${maxAantalBlokken}`
        validationError = {
          maxAantalBlokken: validatieBericht,
        }
      }
    }

    if (toastService) {
      if (validationError) {
        toastService.error(validationError.maxAantalBlokken)
      } else {
        toastService.hide(validatieBericht)
      }
    }

    return validationError
  }
}
