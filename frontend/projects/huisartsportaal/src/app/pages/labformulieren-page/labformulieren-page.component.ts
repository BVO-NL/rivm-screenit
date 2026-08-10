/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { Component, inject, signal, WritableSignal } from '@angular/core'
import {
  DsCardComponent,
  DsCardConfig,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsPageEvent,
  DsPaginatorComponent,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSortModule } from '@angular/material/sort'
import { AanvraagDto, AanvraagStatus, getAanvraagStatusText } from '../../models/AanvraagDto'
import { LabformulierenService } from '../../services/labformulieren/labformulieren.service'
import { DatePipe } from '@angular/common'
import { PagineringDto } from '../../models/PagineringDto'
import { SorteerParameter } from '../../models/SorteerParameter'
import { SorteerRichting } from '../../models/SorteerRichting'
import { take } from 'rxjs'

@Component({
  selector: 'app-labformulieren-page',
  imports: [
    DsCardComponent,
    DsTableComponent,
    DsHeaderCell,
    DsHeaderCellDef,
    DsColumnDef,
    DsCell,
    DsCellDef,
    MatSortModule,
    DsHeaderRowDef,
    DsHeaderRowComponent,
    DsRowDef,
    DsRowComponent,
    DatePipe,
    DsPaginatorComponent,
  ],
  templateUrl: './labformulieren-page.component.html',
  styleUrl: './labformulieren-page.component.scss',
})
export class LabformulierenPageComponent {
  private readonly labformulierenService = inject(LabformulierenService)
  displayedColumns = ['aanvraagDatum', 'aantal', 'status', 'statusDatum', 'aangevraagdDoor', 'locatie']
  cardConfig: DsCardConfig = {
    maxHeight: 'calc(100vh - var(--header-height))',
  }
  dataSource: WritableSignal<AanvraagDto[]> = signal([])
  paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 10, totaal: 0 })
  sortering = signal<SorteerParameter>({ veld: 'aanvraagDatum', richting: SorteerRichting.ASC })

  constructor() {
    this.getLabformulieren()
  }

  getAanvraagStatusText(status: AanvraagStatus): string {
    return getAanvraagStatusText[status]
  }

  getLabformulieren() {
    this.labformulierenService
      .getFormulieren(this.paginering(), this.sortering())
      .pipe(take(1))
      .subscribe((response) => {
        this.paginering.update((paginering) => ({ ...paginering, totaal: response.aantalAanvragen }))
        this.dataSource.set(response.aanvragen)
      })
  }

  handlePageChange($event: DsPageEvent): void {
    const huidigPaginaGrootte = this.paginering().paginaGrootte
    const paginaGrootteGewijzigd = $event.pageSize !== huidigPaginaGrootte

    const paginaIndex = paginaGrootteGewijzigd ? 0 : $event.pageIndex

    this.paginering.update((paginering) => ({
      ...paginering,
      paginaNummer: paginaIndex + 1,
      paginaGrootte: $event.pageSize,
    }))

    this.getLabformulieren()
  }
}
