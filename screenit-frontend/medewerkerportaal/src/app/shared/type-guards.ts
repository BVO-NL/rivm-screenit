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
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'

export function isColonBlokkade(tijdslot: ColonTijdslot): tijdslot is ColonBlokkade {
  return Object.prototype.hasOwnProperty.call(tijdslot, 'omschrijving')
}

export function isColonAfspraakslot(tijdslot: ColonTijdslot): tijdslot is ColonAfspraakslot {
  return Object.prototype.hasOwnProperty.call(tijdslot, 'capaciteitMeeBepaald')
}
