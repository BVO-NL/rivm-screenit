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
import { BriefType } from '../types/algemeen/enum/brief-type'
import { BriefCategorie } from '../types/algemeen/enum/brief-categorie'

export function getCategorieVanBriefType(briefType: BriefType): BriefCategorie {
  const bvo = briefType.split('_')[0]

  if (isBriefCategorieSleutel(bvo)) {
    return BriefCategorie[bvo]
  }

  if (briefType.startsWith('CLIENT_BEZWAAR')) {
    return BriefCategorie.BEZWAAR
  }

  if (briefType.startsWith('CLIENT_')) {
    return BriefCategorie.ALGEMEEN
  }

  throw new Error(`Onbekend briefType: ${briefType}`)
}

function isBriefCategorieSleutel(waarde: string): waarde is keyof typeof BriefCategorie {
  return waarde in BriefCategorie
}
