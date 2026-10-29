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
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'

export const heeftAlleBevolkingsonderzoeken = (bevolkingsonderzoeken: Bevolkingsonderzoek[]): boolean =>
  Object.values(Bevolkingsonderzoek).every((bevolkingsonderzoek) => bevolkingsonderzoeken.includes(bevolkingsonderzoek))

export const alleenDarmkanker = (bevolkingsonderzoeken: Bevolkingsonderzoek[]): boolean => isAlleen(bevolkingsonderzoeken, Bevolkingsonderzoek.COLON)

export const alleenBaarmoederhalskanker = (bevolkingsonderzoeken: Bevolkingsonderzoek[]): boolean => isAlleen(bevolkingsonderzoeken, Bevolkingsonderzoek.CERVIX)

export const alleenBorstkanker = (bevolkingsonderzoeken: Bevolkingsonderzoek[]): boolean => isAlleen(bevolkingsonderzoeken, Bevolkingsonderzoek.MAMMA)

const isAlleen = (bevolkingsonderzoeken: Bevolkingsonderzoek[], bevolkingsonderzoek: Bevolkingsonderzoek): boolean =>
  bevolkingsonderzoeken.length === 1 && bevolkingsonderzoeken[0] === bevolkingsonderzoek
