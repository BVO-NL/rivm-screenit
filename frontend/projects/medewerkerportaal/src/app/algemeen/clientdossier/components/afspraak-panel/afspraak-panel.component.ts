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
import { Component, computed, input, output } from '@angular/core'
import { faCalendar } from '@fortawesome/pro-solid-svg-icons'
import { DsBadgeComponent, DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective } from '@topicus-rgp-ds/web'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { DatumNaarWeekPipe } from '@shared/pipes/datum-naar-week/datum-naar-week.pipe'
import { NL_LONG_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ClientAfspraakEnActies } from '@shared/types/algemeen/client-afspraak-en-acties'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@shared/types/bevolkingsonderzoek'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'

@Component({
  selector: 'app-afspraak-panel',
  imports: [DatumNaarWeekPipe, DatumTijdPipe, DsBadgeComponent, DsButtonComponent, DsCardComponent, EmptyStatePanelComponent, DsFooterActionsRightDirective],
  templateUrl: './afspraak-panel.component.html',
  styleUrl: './afspraak-panel.component.scss',
})
export class AfspraakPanel {
  afspraakEnActies = input<ClientAfspraakEnActies | null>(null)
  protected readonly afspraak = computed(() => this.afspraakEnActies()?.afspraak as ColonIntakeafspraakDto)
  bvo = input.required<Bevolkingsonderzoek>()
  protected readonly magNieuweAfspraakMaken = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.MAKEN) ?? false)
  protected readonly magAfspraakWijzigen = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.VERPLAATSEN) ?? false)

  protected readonly titel = computed(() => bevolkingsonderzoekLabels[this.bvo()])
  tijdstipWijzigen = output<ClientAfspraakDto>()

  protected readonly faCalendar = faCalendar
  protected readonly NL_LONG_DATE_FORMAT = NL_LONG_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT

  protected isColonIntakeafspraak(): boolean {
    return this.afspraakEnActies()!.type === Bevolkingsonderzoek.COLON
  }

  openTijdstipWijzigenPopup(afspraak: ClientAfspraakDto) {
    this.tijdstipWijzigen.emit(afspraak)
  }
}
