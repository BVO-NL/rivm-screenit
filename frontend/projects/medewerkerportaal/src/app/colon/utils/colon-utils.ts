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
import { HttpErrorResponse } from '@angular/common/http'
import { ColonTijdslotType } from '@shared/types/colon/colon-tijdslot-type'

export function bepaalFoutmeldingBijOpslaanTijdslot(err: HttpErrorResponse, type: ColonTijdslotType): string {
  const specifiekeFoutmeldingen = ['overlap', 'maximum toegestane', 'niet toegestaan', 'capaciteitsberekening']
  const isSpecifiekeFout = specifiekeFoutmeldingen.some((melding) => err?.error?.message?.includes(melding))

  if (isSpecifiekeFout && err.error.additionalInfo) {
    return `${err.error.message} ${err.error.additionalInfo}`
  } else if (isSpecifiekeFout) {
    return err.error.message
  } else {
    const lidwoord = type === ColonTijdslotType.AFSPRAAKSLOT ? 'het' : 'de'
    return `Er is een fout opgetreden bij het opslaan van ${lidwoord} ${type.toLowerCase()}. Probeer het opnieuw.`
  }
}
