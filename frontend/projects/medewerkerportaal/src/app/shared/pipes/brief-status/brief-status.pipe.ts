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
import { BriefDto } from '@shared/types/algemeen/dto/brief.dto'
import { isAfter } from 'date-fns'

@Pipe({
  name: 'briefStatus',
})
export class BriefStatusPipe implements PipeTransform {
  private meestRecenteBriefPerGroep(brieven: BriefDto[]) {
    const meestRecentePerGroep = new Map<number, BriefDto>()
    for (const brief of brieven) {
      const groepId = brief.herdrukBrief?.id ?? brief.id
      const huidigeMeestRecente = meestRecentePerGroep.get(groepId)
      if (!huidigeMeestRecente || isAfter(brief.creatieDatum, huidigeMeestRecente.creatieDatum)) {
        meestRecentePerGroep.set(groepId, brief)
      }
    }
    return meestRecentePerGroep
  }

  transform(brief: BriefDto, brieven: BriefDto[]): string {
    const groepId = brief.herdrukBrief?.id ?? brief.id
    if (this.meestRecenteBriefPerGroep(brieven).get(groepId)?.id !== brief.id) {
      return 'Vervangen'
    }
    if (brief.tegengehouden) {
      return 'Tegengehouden'
    }
    if (brief.vervangen) {
      return 'Vervangen'
    }
    if (brief.gegenereerd) {
      return 'Verzonden'
    }
    return 'Aangemaakt'
  }
}
