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
import { Component, computed, inject } from '@angular/core'
import {
  DsButtonComponent,
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
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { OnderzoeksresultatenActieDto } from '@shared/types/algemeen/dto/verwijderde-onderzoeksresultaten.dto'
import { ClientService } from '@algemeen/services/client/client.service'
import { BvoLabelPipe } from '@shared/pipes/bvo-label/bvo-label.pipe'
import { Dialog } from '@angular/cdk/dialog'
import { VerwijderdeOnderzoeksresultatenDialogComponent } from './onderzoeksresultaten-dialog/verwijderde-onderzoeksresultaten-dialog.component'

@Component({
  selector: 'app-verwijderde-onderzoeksresultaten-panel',
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
    DsButtonComponent,
    DatumTijdPipe,
    BvoLabelPipe,
  ],
  templateUrl: './verwijderde-onderzoeksresultaten-panel.component.html',
  host: {
    '[class.display-none]': 'onderzoeksresultaten().length === 0',
  },
})
export class VerwijderdeOnderzoeksresultatenPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)
  protected onderzoeksresultaten = computed(() => this.clientService.select('client')().onderzoeksresultatenActies)
  protected readonly displayedColumns = ['bevolkingsonderzoek', 'datum', 'bekijken']

  protected bekijkOnderzoeksresultaten(onderzoeksresultaten: OnderzoeksresultatenActieDto): void {
    this.dialog.open(VerwijderdeOnderzoeksresultatenDialogComponent, { data: onderzoeksresultaten })
  }
}
