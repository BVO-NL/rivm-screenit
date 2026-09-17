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
import { Component, input } from '@angular/core'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { DsDescriptionCustomTemplateDirective, DsDescriptionsComponent, DsSummaryPanelComponent } from '@topicus-rgp-ds/web'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { NL_DAY_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'

@Component({
  selector: 'app-colon-client-afspraak-samenvatting',
  imports: [DsSummaryPanelComponent, DsDescriptionsComponent, DatumTijdPipe, DsDescriptionCustomTemplateDirective, EnumLabelPipe, LegeWaardePipe],
  templateUrl: './colon-client-afspraak-samenvatting.component.html',
  styleUrl: './colon-client-afspraak-samenvatting.component.scss',
})
export class ColonClientAfspraakSamenvattingComponent {
  request = input<ColonAfspraakMakenRequestDto | null>()

  protected readonly NL_DAY_DATE_FORMAT = NL_DAY_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT
  protected readonly briefTypeLabels = briefTypeLabels
}
