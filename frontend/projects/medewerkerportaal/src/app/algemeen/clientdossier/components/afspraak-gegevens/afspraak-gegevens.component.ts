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
import { Component, inject, input } from '@angular/core'
import { DsBadgeComponent } from '@topicus-rgp-ds/web'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { DatumNaarWeekPipe } from '@shared/pipes/datum-naar-week/datum-naar-week.pipe'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'
import { NL_LONG_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-afspraak-gegevens',
  imports: [DatumNaarWeekPipe, DatumTijdPipe, DsBadgeComponent, LegeWaardePipe],
  templateUrl: './afspraak-gegevens.component.html',
  styleUrl: './afspraak-gegevens.component.scss',
})
export class AfspraakGegevensComponent {
  afspraak = input.required<ClientAfspraakDto>()
  digitaal = input(false)

  protected readonly strings = inject(StringsService).getStrings()
  protected readonly NL_LONG_DATE_FORMAT = NL_LONG_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT
}
