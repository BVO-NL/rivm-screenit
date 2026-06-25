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
import { Component, inject } from '@angular/core'
import {
  DsCardComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsNoDataRow,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { ProjectService } from '@algemeen/services/project/project.service'
import { toSignal } from '@angular/core/rxjs-interop'
import { ClientService } from '@algemeen/clientdossier/services/client/client.service'
import { ActiefIndicatorComponent } from '@shared/components/actief-indicator/actief-indicator.component'
import { BvoIndicatorComponent } from '@shared/components/bvo-indicator/bvo-indicator.component'

@Component({
  selector: 'app-projecten-panel',
  imports: [
    DsCardComponent,
    DsTableComponent,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderCellDef,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsRowComponent,
    DsRowDef,
    DsNoDataRow,
    ActiefIndicatorComponent,
    BvoIndicatorComponent,
  ],
  templateUrl: './projecten-panel.component.html',
  host: {
    '[class.display-none]': 'projecten().length === 0',
  },
})
export class ProjectenPanelComponent {
  private readonly projectService = inject(ProjectService)
  private readonly clientService = inject(ClientService)

  private readonly clientId = this.clientService.select('client')().id
  protected projecten = toSignal(this.projectService.getProjectenVoorClient(this.clientId!), { initialValue: [] })
  protected readonly displayedColumns = ['bevolkingsonderzoeken', 'naam', 'status']
}
