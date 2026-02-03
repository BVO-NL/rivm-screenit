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
import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms'

export const aantalBestandenValidator = (maxAantal: number): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const files: FileList = control.value
    if (files && files.length > maxAantal) {
      return { aantalBestanden: `Er mogen maximaal ${maxAantal} bestanden worden geüpload.` }
    }
    return null
  }
}

export const extensieValidator = (toegestaneExtensies: string[]): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const files: FileList = control.value
    if (files) {
      for (let i = 0; i < files.length; i++) {
        const file = files[i]
        const extensie = file.name.split('.').pop()?.toLowerCase()
        if (extensie && !toegestaneExtensies.map((ext) => ext.toLowerCase()).includes(extensie)) {
          return { extensie: `'${file.name}' heeft niet de juiste extensie. U moet een bestand met extensie '${toegestaneExtensies}' uploaden.` }
        }
      }
    }
    return null
  }
}
