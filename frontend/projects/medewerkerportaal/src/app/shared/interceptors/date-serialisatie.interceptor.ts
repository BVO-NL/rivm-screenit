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
import { HttpHandlerFn, HttpInterceptorFn, HttpRequest } from '@angular/common/http'
import { isValid } from 'date-fns'
import { formatLokaleDatumOfDatumTijd } from '@shared/utils/date-utils'

export const dateSerialisatieInterceptor: HttpInterceptorFn = (req: HttpRequest<unknown>, next: HttpHandlerFn) => {
  if (req.body == null || typeof req.body !== 'object') {
    return next(req)
  }
  return next(req.clone({ body: zetDatesOmNaarLokaleString(req.body) }))
}

function zetDatesOmNaarLokaleString(waarde: unknown): unknown {
  if (waarde instanceof Date) {
    return isValid(waarde) ? formatLokaleDatumOfDatumTijd(waarde) : null
  }
  if (Array.isArray(waarde)) {
    return waarde.map(zetDatesOmNaarLokaleString)
  }
  if (waarde !== null && typeof waarde === 'object' && Object.getPrototypeOf(waarde) === Object.prototype) {
    return Object.fromEntries(Object.entries(waarde).map(([sleutel, veldWaarde]) => [sleutel, zetDatesOmNaarLokaleString(veldWaarde)]))
  }
  return waarde
}
