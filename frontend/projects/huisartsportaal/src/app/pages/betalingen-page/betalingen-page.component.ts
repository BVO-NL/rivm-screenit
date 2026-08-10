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
import { AfterViewInit, Component, computed, DestroyRef, inject, signal, viewChild, WritableSignal } from '@angular/core'
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
import { MatSort, MatSortHeader, Sort } from '@angular/material/sort'
import { BetalingDto } from '../../models/BetalingDto'
import { BetalingenService } from '../../services/betalingen/betalingen.service'
import { BetalingenFilterEvent } from '../../models/BetalingenFilterEvent'
import { BetalingenFilterComponent } from './components/betalingen-filter/betalingen-filter.component'
import { PagineringDto } from '../../models/PagineringDto'
import { SorteerParameter } from '../../models/SorteerParameter'
import { SorteerRichting } from '../../models/SorteerRichting'
import { NL_DATE_FORMAT } from '../../utils/constants'
import { take } from 'rxjs'
import { CurrencyPipe, DatePipe, NgClass } from '@angular/common'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { formatDate } from 'date-fns'
import writeXlsxFile from 'write-excel-file/browser'

@Component({
  selector: 'app-betalingen-page',
  imports: [
    DsCardComponent,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCellDef,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsPaginatorComponent,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    MatSort,
    MatSortHeader,
    BetalingenFilterComponent,
    DatePipe,
    NgClass,
    CurrencyPipe,
    DsButtonComponent,
    DsFooterActionsRightDirective,
  ],
  templateUrl: './betalingen-page.component.html',
  styles: `
    .debet {
      background-color: rgba(250, 1, 1, 0.05);
    }

    .credit {
      background-color: rgba(1, 250, 1, 0.05);
    }
  `,
  providers: [CurrencyPipe],
})
export class BetalingenPageComponent implements AfterViewInit {
  private betalingenService = inject(BetalingenService)
  private readonly destroyRef = inject(DestroyRef)
  private readonly sort = viewChild(MatSort)
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  private readonly currencyPipe = inject(CurrencyPipe)

  displayedColumns = ['huisartsLocatie', 'clientNaam', 'monsterId', 'bedrag', 'betalingsdatum', 'betalingsKenmerk', 'regio']
  cardConfig: DsCardConfig = {
    maxHeight: 'calc(100vh - var(--header-height))',
  }
  filter?: BetalingenFilterEvent

  paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 10, totaal: 0 })
  sortering = signal<SorteerParameter>({ veld: 'huisartsLocatie.naam', richting: SorteerRichting.ASC })
  dataSource: WritableSignal<BetalingDto[]> = signal([])
  totaalBedrag = computed(() => this.dataSource().reduce((totaal, betaling) => totaal + (betaling.bedrag ?? 0), 0))

  constructor() {
    this.getBetalingen()
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

        this.getBetalingen()
      })
  }

  onFilterChanged(event: BetalingenFilterEvent) {
    this.filter = event
    this.getBetalingen()
  }

  getBetalingen() {
    this.betalingenService
      .getBetalingen(this.paginering(), this.sortering(), this.filter)
      .pipe(take(1))
      .subscribe((response) => {
        this.paginering.update((paginering) => ({ ...paginering, totaal: response.aantalBetalingen }))
        this.dataSource.set(response.betalingen)
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

    this.getBetalingen()
  }

  async exporteren() {
    const toCell = (value: string | number | null | undefined) => ({ value: value ?? undefined })

    const rows = [
      [
        toCell('Huisartslocatie'),
        toCell('Naam cliënt'),
        toCell('Monster-id'),
        toCell('Bedrag'),
        toCell('Datum betaling'),
        toCell('Betalingskenmerk'),
        toCell('Screeningsorganisatie'),
      ],
      ...this.dataSource().map((betaling) => [
        toCell(betaling.verrichting?.huisartsLocatieNaam),
        toCell(betaling.verrichting?.clientNaam),
        toCell(betaling.verrichting?.monsterId),
        toCell(this.currencyPipe.transform(betaling.bedrag, 'EUR', '')),
        toCell(betaling.betalingsdatum ? formatDate(betaling.betalingsdatum, NL_DATE_FORMAT) : ''),
        toCell(betaling.betalingsKenmerk),
        toCell(betaling.verrichting?.regio),
      ]),
    ]
    await writeXlsxFile(rows).toFile('overzicht-betalingen.xlsx')
  }
}
