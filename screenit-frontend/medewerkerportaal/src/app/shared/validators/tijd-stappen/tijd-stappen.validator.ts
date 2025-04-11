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

export const tijdStappenValidator: ValidatorFn = (tijdCtrl: AbstractControl): ValidationErrors | null => {
  if (!tijdCtrl.value || !tijdCtrl.dirty) {
    return null
  }
  const minutenString = tijdCtrl.value.split(':')[1]
  const minutes = Number(minutenString)
  return minutes % 5 !== 0 ? { tijdStappen: 'Minuten van start mogen alleen een veelvoud van 5 zijn.' } : null
}
