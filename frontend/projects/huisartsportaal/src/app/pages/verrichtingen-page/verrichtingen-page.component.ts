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
import { AfterViewInit, Component, DestroyRef, inject, signal, viewChild, WritableSignal } from '@angular/core'
import {
  DsButtonComponent,
  DsCardComponent,
  DsCardConfig,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsFooterActionsRightDirective,
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
import { MatSort, MatSortModule, Sort } from '@angular/material/sort'
import { DatePipe } from '@angular/common'
import { VerrichtingDto } from '../../models/VerrichtingDto'
import { VerrichtingenService } from '../../services/verrichtingen/verrichtingen.service'
import { VerrichtingenFilterComponent } from './components/verrichtingen-filter/verrichtingen-filter.component'
import { VerrichtingenFilterEvent } from '../../models/VerrichtingenFilterEvent'
import { SorteerParameter } from '../../models/SorteerParameter'
import { SorteerRichting } from '../../models/SorteerRichting'
import { NL_DATE_FORMAT } from '../../utils/constants'
import { PagineringDto } from '../../models/PagineringDto'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { take } from 'rxjs'
import writeXlsxFile from 'write-excel-file/browser'
import { formatDate } from 'date-fns'

@Component({
  selector: 'app-verrichtingen-page',
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
    VerrichtingenFilterComponent,
    DsButtonComponent,
    DsFooterActionsRightDirective,
  ],
  templateUrl: './verrichtingen-page.component.html',
})
export class VerrichtingenPageComponent implements AfterViewInit {
  private readonly verrichtingenService = inject(VerrichtingenService)
  private readonly destroyRef = inject(DestroyRef)
  private readonly sort = viewChild(MatSort)
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT

  displayedColumns = ['huisartsLocatie', 'clientNaam', 'monsterId', 'datumUitstrijkje', 'verrichtingsDatum', 'formulierOntvangstDatum', 'regio']
  cardConfig: DsCardConfig = {
    maxHeight: 'calc(100vh - var(--header-height))',
  }
  filter?: VerrichtingenFilterEvent

  dataSource: WritableSignal<VerrichtingDto[]> = signal([])
  paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 10, totaal: 0 })
  sortering = signal<SorteerParameter>({ veld: 'huisartsLocatie.naam', richting: SorteerRichting.ASC })

  constructor() {
    this.getVerrichtingen()
  }

  ngAfterViewInit(): void {
    this.sort()
      ?.sortChange.pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe((sortEvent: Sort) => {
        if (!sortEvent.direction) {
          this.sortering.set({ veld: 'huisartsLocatie.naam', richting: SorteerRichting.ASC })
        } else {
          this.sortering.set({
            veld: sortEvent.active,
            richting: sortEvent.direction === 'asc' ? SorteerRichting.ASC : SorteerRichting.DESC,
          })
        }

        this.paginering.update((paginering) => ({ ...paginering, paginaNummer: 1 }))

        this.getVerrichtingen()
      })
  }

  onFilterChanged(event?: VerrichtingenFilterEvent) {
    this.filter = event
    this.getVerrichtingen()
  }

  getVerrichtingen(): void {
    this.verrichtingenService
      .getVerrichtingen(this.paginering(), this.sortering(), this.filter)
      .pipe(take(1))
      .subscribe((response) => {
        this.paginering.update((paginering) => ({ ...paginering, totaal: response.aantalVerrichtingen }))
        this.dataSource.set(response.verrichtingen)
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

    this.getVerrichtingen()
  }

  async exporteren() {
    const toCell = (value: string | number | null | undefined) => ({ value: value ?? undefined })

    const rows = [
      [
        toCell('Huisartslocatie'),
        toCell('Naam cliënt'),
        toCell('Monster-id'),
        toCell('Datum uitstrijk'),
        toCell('Datum verrichting'),
        toCell('Ontvangst formulier'),
        toCell('Screeningsorganisatie'),
      ],
      ...this.dataSource().map((verrichting) => [
        toCell(verrichting.huisartsLocatie?.naam),
        toCell(verrichting.clientNaam),
        toCell(verrichting.monsterId),
        toCell(verrichting.datumUitstrijkje ? formatDate(verrichting.datumUitstrijkje, NL_DATE_FORMAT) : ''),
        toCell(verrichting.verrichtingsDatum ? formatDate(verrichting.verrichtingsDatum, NL_DATE_FORMAT) : ''),
        toCell(verrichting.formulierOntvangstDatum ? formatDate(verrichting.formulierOntvangstDatum, NL_DATE_FORMAT) : ''),
        toCell(verrichting.regio),
      ]),
    ]
    await writeXlsxFile(rows).toFile('overzicht-verrichtingen.xlsx')
  }
}
