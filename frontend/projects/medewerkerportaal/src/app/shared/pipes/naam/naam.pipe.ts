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
import { Pipe, PipeTransform } from '@angular/core'
import { PersoonDto } from '@shared/types/algemeen/dto/persoon.dto'
import { NaamUtils } from '@shared/utils/naam-utils'

export enum NaamTransform {
  VoorlettersTussenvoegselAanspreekvormAchternaam = 'voorlettersTussenvoegselAanspreekvormAchternaam',
  Volledig = 'volledig',
  Medewerker = 'medewerker',
}

@Pipe({
  name: 'naam',
})
export class NaamPipe implements PipeTransform {
  transform(value: PersoonDto | undefined, transform: NaamTransform): string | null {
    if (!value) {
      return ''
    }

    switch (transform) {
      case NaamTransform.VoorlettersTussenvoegselAanspreekvormAchternaam:
        return NaamUtils.titelVoorlettersTussenvoegselEnAanspreekAchternaam(value)
      case NaamTransform.Volledig:
        return NaamUtils.getNaamVolledig(value)
      case NaamTransform.Medewerker:
        return NaamUtils.getNaamMedewerker(value)
      default:
        return ''
    }
  }
}
