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
import { BezwaarService } from '@/algemeen/services/bezwaar/bezwaar.service'
import { BezwaarGroupViewWrapper } from '@/shared/types/algemeen/bezwaar-group-view-wrapper'
import { BezwaarMomentDto } from '@/shared/types/algemeen/dto/bezwaar-moment.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@/shared/types/bevolkingsonderzoek'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'
import { NgClass } from '@angular/common'
import { Component, computed, inject, input } from '@angular/core'
import { faFile } from '@fortawesome/pro-solid-svg-icons'

const GROEP_VOLGORDE: Record<string, number> = {
  ALGEMEEN: 0,
  [Bevolkingsonderzoek.CERVIX]: 1,
  [Bevolkingsonderzoek.MAMMA]: 2,
}

@Component({
  selector: 'app-bezwaar-lijst',
  imports: [NgClass, EmptyStatePanelComponent],
  templateUrl: './bezwaar-lijst.component.html',
  styles: `
    .groep-label {
      font-weight: 600;
      color: var(--ds-secondary-text-color);
    }

    ul {
      list-style-type: disc;
    }
  `,
})
export class BezwaarLijstComponent {
  private readonly bezwaarService = inject(BezwaarService)
  protected readonly faFile = faFile

  horizontaal = input<boolean>(false)
  bezwaarMoment = input<BezwaarMomentDto | undefined>()
  subtekst = input<string>('')
  bezwaarGroupViewWrappers = computed(() =>
    [...this.bezwaarService.getBezwaarGroupViewWrappers(this.bezwaarMoment(), true)].sort(
      (a: BezwaarGroupViewWrapper, b: BezwaarGroupViewWrapper) => (GROEP_VOLGORDE[a.key] ?? Number.MAX_SAFE_INTEGER) - (GROEP_VOLGORDE[b.key] ?? Number.MAX_SAFE_INTEGER),
    ),
  )

  protected getGroepLabel(groep: { key: string; bevolkingsonderzoek?: Bevolkingsonderzoek }) {
    if (!groep.bevolkingsonderzoek) {
      return 'Algemeen'
    }

    return bevolkingsonderzoekLabels[groep.bevolkingsonderzoek] ?? groep.key
  }
}
