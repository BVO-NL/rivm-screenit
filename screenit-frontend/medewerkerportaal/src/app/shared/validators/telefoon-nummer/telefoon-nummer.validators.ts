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
import { AbstractControl, ValidationErrors } from '@angular/forms'
import { isStringNullOfLeeg } from '@shared/utils/string-utils'

const verwijderSpatiesEnStreepjes = (waarde: string): string => (waarde ? waarde.replace(/[\s-]+/g, '') : '')

export const mobieleTelefoonValidator = (control: AbstractControl): ValidationErrors | null => {
  const waardeZonderSpatieOfStreepje = verwijderSpatiesEnStreepjes(control.value)

  if (!control.dirty) {
    return null
  }

  const mobielTelefoonRegex = new RegExp(/^(?:(?:\+|00)31|0)6-?\d{8}$/)
  return !isStringNullOfLeeg(waardeZonderSpatieOfStreepje) && mobielTelefoonRegex.test(waardeZonderSpatieOfStreepje) ? null : { mobielTelefoon: true }
}

export const normaalTelefoonnummerValidator = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty) {
    return null
  }

  const waardeZonderSpatieOfStreepje = verwijderSpatiesEnStreepjes(control.value)

  const vastNummerPattern = new RegExp(/^(0[0-9]{9})|(0[0-9]{2}[0-9]{7})|(0[0-9]{3}[0-9]{6})$/)
  const informatieNummerPattern = new RegExp(/^0[89]00[0-9]{4}([0-9]{3})?$/)
  const buitenlandsNummerPattern = new RegExp(/^(\+|00)[0-9]{4,15}$/)
  return !isStringNullOfLeeg(waardeZonderSpatieOfStreepje) &&
    (vastNummerPattern.test(waardeZonderSpatieOfStreepje) ||
      informatieNummerPattern.test(waardeZonderSpatieOfStreepje) ||
      buitenlandsNummerPattern.test(waardeZonderSpatieOfStreepje))
    ? null
    : { telefoon: true }
}
