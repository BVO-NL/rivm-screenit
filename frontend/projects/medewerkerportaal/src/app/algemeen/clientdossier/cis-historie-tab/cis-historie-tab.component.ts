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
import { Component, computed, effect, inject, Signal, viewChildren } from '@angular/core'
import { CervixCisHistorieService } from './services/cervix-cis-historie.service'
import { CervixCisHistorieDto } from '@shared/types/cervix/cervix-cis-historie.dto'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'
import {
  DsBadgeComponent,
  DsBadgeType,
  DsCardComponent,
  DsCardHeaderBadgeDirective,
  DsCardHeaderSummaryDirectives,
  DsIconComponent,
  DsPageSummaryTagComponent,
  DsToggleComponent,
} from '@topicus-rgp-ds/web'
import { DatePipe } from '@angular/common'
import { LONG_TIME_FORMAT, NL_DATE_FORMAT } from '@shared/constants'
import { ClientService } from '@algemeen/services/client/client.service'
import { FormControl, ReactiveFormsModule } from '@angular/forms'
import { faCalendar } from '@fortawesome/pro-solid-svg-icons'
import { format } from 'date-fns'

@Component({
  selector: 'app-cis-historie-tab',
  imports: [
    EmptyStatePanelComponent,
    DsCardComponent,
    DsCardHeaderSummaryDirectives,
    DatePipe,
    DsToggleComponent,
    ReactiveFormsModule,
    DsIconComponent,
    DsCardHeaderBadgeDirective,
    DsBadgeComponent,
    DsPageSummaryTagComponent,
  ],
  templateUrl: './cis-historie-tab.component.html',
  styleUrl: './cis-historie-tab.component.scss',
  providers: [CervixCisHistorieService],
})
export class CisHistorieTabComponent {
  private readonly cisHistorieService = inject(CervixCisHistorieService)
  private readonly clientService = inject(ClientService)

  protected readonly cisHistorie: Signal<CervixCisHistorieDto> = toSignal(this.cisHistorieService.getCisHistorie(this.clientService.clientId()), { initialValue: { rondes: {} } })
  protected readonly rondes: Signal<string[]> = computed(() => this.cisHistorieService.getRondes(this.cisHistorie()))
  protected readonly heeftHistorie = computed(() => this.rondes().length > 0)

  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly LOCAL_TIME_FORMAT = LONG_TIME_FORMAT
  protected readonly uitklappenCtrl = new FormControl<boolean>(false)
  protected readonly faCalendar = faCalendar
  private readonly cards = viewChildren(DsCardComponent)

  constructor() {
    this.uitklappenCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe(() => {
      this.cards().forEach((card) => card.toggleCardState())
    })

    effect(() => {
      if (this.cards() && this.cards().length > 0) {
        this.cards()[0].isExpanded.set(true)
      }
    })
  }

  protected berekenRondeDuur(ronde: string): string {
    const historie = this.cisHistorie().rondes[ronde]
    if (historie.length === 0) {
      return ''
    }

    const eersteRonde = historie[0]
    const laatsteRonde = historie[historie.length - 1]

    if (historie.length === 1) {
      return format(eersteRonde.datum, NL_DATE_FORMAT)
    } else {
      return `${format(laatsteRonde.datum, NL_DATE_FORMAT)} t/m ${format(eersteRonde.datum, NL_DATE_FORMAT)}`
    }
  }

  protected getBadgeType(ronde: string): DsBadgeType {
    switch (ronde) {
      case 'memo':
        return 'info'
      case 'bezwaar':
        return 'warning'
      default:
        return 'default'
    }
  }

  protected toonRondeDuur(ronde: string): boolean {
    return ronde !== 'memo' && ronde !== 'bezwaar'
  }
}
