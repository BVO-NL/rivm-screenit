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
import { AfterViewInit, ChangeDetectorRef, Component, DestroyRef, effect, inject, input, model, output, viewChild } from '@angular/core'
import {
  DsAbstractSelectableTable,
  DsCell,
  DsCellDef,
  DsCheckboxComponent,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsPageEvent,
  DsPaginatorComponent,
  DsRowDef,
  DsSelectableRowComponent,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSort, MatSortHeader, Sort } from '@angular/material/sort'
import { MammaFotobesprekingOnderzoekDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek.dto'
import { NL_DATE_FORMAT, NL_DATE_TIME_FORMAT } from '@shared/constants'
import { DatePipe } from '@angular/common'
import { PagineringDto } from '@shared/types/paginering'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { SorteerRichting } from '@shared/types/sorteer-richting'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { BooleanPipe } from '@shared/pipes/boolean/boolean.pipe'
import { FormsModule } from '@angular/forms'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { mammaOnderzoekRedenFotobesprekingLabels } from '@shared/types/mamma/enum/mamma-onderzoek-reden-fotobespreking'
import { mammaFollowUpConclusieStatusLabel } from '@shared/types/mamma/enum/mamma-follow-up-conclusie-status'
import { mammaLaesieTypeLabels } from '@shared/types/mamma/enum/mamma-laesie-type'
import { mammaLezingRedenenFotobesprekingMbberLabels } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-mbber'
import { mammaLezingRedenenFotobesprekingRadioloogLabels } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-radioloog'

@Component({
  selector: 'app-mamma-fotobespreking-tabel',
  imports: [
    DsTableComponent,
    MatSort,
    DsHeaderCellDef,
    DsColumnDef,
    DsCellDef,
    DsCell,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsRowDef,
    DsHeaderRowDef,
    DsPaginatorComponent,
    MatSortHeader,
    DatePipe,
    BooleanPipe,
    DsCheckboxComponent,
    FormsModule,
    DsSelectableRowComponent,
    EnumLabelPipe,
  ],
  templateUrl: './mamma-fotobespreking-tabel.component.html',
})
export class MammaFotobesprekingTabelComponent extends DsAbstractSelectableTable<MammaFotobesprekingOnderzoekDto> implements AfterViewInit {
  paginering = model.required<PagineringDto>()
  sortering = model.required<SorteerParameterDto>()
  onderzoeken = input<MammaFotobesprekingOnderzoekDto[]>([])
  clientIds = input<number[] | undefined>([])
  getOnderzoeken = output<void>()
  selectionChanged = output<MammaFotobesprekingOnderzoekDto[]>()
  private sort = viewChild(MatSort)
  private readonly destroyRef = inject(DestroyRef)
  private readonly cdr = inject(ChangeDetectorRef)

  displayedColumns = [
    'selection',
    'naam',
    'geboortedatum',
    'bsn',
    'medewerker',
    'onderzoeksdatum',
    'redenFotobesprekingDoorMbber',
    'redenFotobesprekingMetMbber',
    'redenFotobesprekingDoorRadioloog',
    'redenDoorverwijzing',
    'discrepantie',
    'followUp',
  ]
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly mammaOnderzoekRedenFotobesprekingLabels = mammaOnderzoekRedenFotobesprekingLabels
  protected readonly mammaFollowUpConclusieStatusLabel = mammaFollowUpConclusieStatusLabel
  protected readonly mammaLaesieTypeLabels = mammaLaesieTypeLabels
  protected readonly mammaLezingRedenenFotobesprekingMbberLabels = mammaLezingRedenenFotobesprekingMbberLabels
  protected readonly mammaLezingRedenenFotobesprekingRadioloogLabels = mammaLezingRedenenFotobesprekingRadioloogLabels

  constructor() {
    super()
    this.setupTableDataSource([])

    effect(() => {
      if (this.onderzoeken()) {
        this.dataSource.data = this.onderzoeken()
        this.selection.clear()

        if (this.clientIds()) {
          this.selection.setSelection(...this.onderzoeken().filter((onderzoek) => this.clientIds()!.includes(onderzoek.clientId)))
        }
      }
    })
  }

  override toggleAll() {
    super.toggleAll()
    this.cdr.markForCheck()
    this.emitSelectieWijziging()
  }

  ngAfterViewInit(): void {
    this.sort()
      ?.sortChange.pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe((sortEvent: Sort) => {
        if (!sortEvent.direction) {
          this.sortering.set({ veld: 'creatieDatum', richting: SorteerRichting.DESC })
        } else {
          this.sortering.set({
            veld: sortEvent.active,
            richting: sortEvent.direction === 'asc' ? SorteerRichting.ASC : SorteerRichting.DESC,
          })
        }

        this.paginering.update((paginering) => ({ ...paginering, paginaNummer: 1 }))

        this.getOnderzoeken.emit()
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

    this.getOnderzoeken.emit()
  }

  selecteerOnderzoek(onderzoek: MammaFotobesprekingOnderzoekDto) {
    if (this.isAlInFotobespreking(onderzoek)) {
      return
    }

    this.selectRow(onderzoek)
    this.emitSelectieWijziging()
  }

  isAlInFotobespreking(onderzoek: MammaFotobesprekingOnderzoekDto) {
    return this.clientIds() != undefined && this.clientIds()!.includes(onderzoek.clientId)
  }

  private emitSelectieWijziging() {
    const newGeselecteerdeOnderzoeken = this.selection.selected.filter((onderzoek) => !this.isAlInFotobespreking(onderzoek))
    this.selectionChanged.emit(newGeselecteerdeOnderzoeken)
  }
}
