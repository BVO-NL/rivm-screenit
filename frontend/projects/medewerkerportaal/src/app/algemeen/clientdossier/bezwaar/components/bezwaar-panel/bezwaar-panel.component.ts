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
import { Component, computed, inject, input } from '@angular/core'
import { DsButtonComponent, DsCardComponent, DsCardHeaderContentDirective, DsIconComponent } from '@topicus-rgp-ds/web'
import { faArrowRight } from '@fortawesome/pro-solid-svg-icons'
import { ClientService } from '@algemeen/services/client/client.service'
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { RouterLink } from '@angular/router'
import { BezwaarLijstComponent } from '../bezwaar-lijst/bezwaar-lijst.component'

@Component({
  selector: 'app-bezwaar-panel',
  imports: [DsCardComponent, DsButtonComponent, DsCardHeaderContentDirective, DsIconComponent, RouterLink, BezwaarLijstComponent],
  templateUrl: './bezwaar-panel.component.html',
  host: {
    '[class.display-none]': '!isZichtbaar()',
    '[class.display-block]': 'isZichtbaar()',
  },
})
export class BezwaarPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly bezwaarMomenten = computed(() => this.clientService.select('client')()?.bezwaarMomenten ?? [])
  protected readonly faArrowRight = faArrowRight
  protected readonly actiefBezwaarMoment = computed(() => this.bezwaarMomenten().find((bm) => bm.status === AanvraagBriefStatus.VERWERKT))
  protected readonly isZichtbaar = computed(
    () =>
      this.bezwaarMomenten().length > 0 &&
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_CLIENT_BEZWAAR],
        level: ToegangLevel.LANDELIJK,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
        actie: Actie.INZIEN,
        required: Required.ALL,
      }),
  )
  opOverzichtPagina = input<boolean>(false)
  titel = input<string>('Actueel')
  subtekst = input<string>('')
}
