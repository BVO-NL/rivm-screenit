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
import { Component, inject, Signal } from '@angular/core'
import {
  DsCardComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { ActiefIndicatorComponent } from '@shared/components/actief-indicator/actief-indicator.component'
import { ClientService } from '@algemeen/clientdossier/services/client/client.service'
import { toSignal } from '@angular/core/rxjs-interop'
import { NL_DATE_FORMAT } from '@shared/constants'
import { DatePipe } from '@angular/common'
import { BvoLabelPipe } from '@shared/pipes/bvo-label/bvo-label.pipe'
import { BvoStatusDto } from '@shared/types/algemeen/dto/bvo-status.dto'

@Component({
  selector: 'app-bvo-status-panel',
  imports: [
    DsCardComponent,
    ActiefIndicatorComponent,
    DsHeaderCellDef,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    DatePipe,
    BvoLabelPipe,
  ],
  templateUrl: './bvo-status-panel.component.html',
})
export class BvoStatusPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly clientId = this.clientService.select('client')().id
  protected bvoStatussen: Signal<BvoStatusDto[]> = toSignal(this.clientService.getBvoStatus(this.clientId!), { initialValue: [] })
  protected readonly displayedColumns = ['bevolkingsonderzoek', 'status', 'vervolg-indicatie']
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
}
