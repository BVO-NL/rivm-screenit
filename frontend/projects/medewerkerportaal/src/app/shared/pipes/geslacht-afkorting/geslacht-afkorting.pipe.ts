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
import { inject, Pipe, PipeTransform } from '@angular/core'
import { Geslacht, geslachtAfkorting } from '../../types/algemeen/enum/geslacht'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Required } from '@shared/types/autorisatie/required'

@Pipe({
  name: 'geslachtAfkorting',
  pure: true,
})
export class GeslachtAfkortingPipe implements PipeTransform {
  private autorisatieService = inject(AutorisatieService)
  transform(geslacht: Geslacht | undefined | null, fallback: string = ''): string {
    if (
      !this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_TOON_GENDERINDETITEIT],
        actie: Actie.INZIEN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
        level: ToegangLevel.LANDELIJK,
        required: Required.ANY,
      })
    ) {
      return ''
    }
    return geslacht ? `(${geslachtAfkorting[geslacht]})` : fallback
  }
}
