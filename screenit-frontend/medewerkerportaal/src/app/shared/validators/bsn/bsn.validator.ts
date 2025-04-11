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

export const bsnValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  const invoer = control.value
  if (!invoer) {
    return null
  }

  const isValid = invoer.length === 9 && /^\d+$/.test(invoer) ? isElfProefVariatie(invoer) : false
  return isValid ? null : { bsn: 'Ingevoerde BSN is niet valide.' }
}

function isElfProefVariatie(bsn: string): boolean {
  let totaal = 0

  for (let i = 0; i < 8; ++i) {
    const cijfer = bsn.substring(i, i + 1)
    totaal += parseInt(cijfer) * (9 - i)
  }

  const rest = bsn.substring(8, 9)
  return totaal !== 0 && totaal % 11 === parseInt(rest)
}
