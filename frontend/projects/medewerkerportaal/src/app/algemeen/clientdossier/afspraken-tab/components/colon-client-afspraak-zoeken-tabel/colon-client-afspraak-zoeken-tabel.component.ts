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
import { AfterViewInit, Component, DestroyRef, effect, inject, input, output, signal, untracked, viewChild, WritableSignal } from '@angular/core'
import {
  DsButtonComponent,
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
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { DatePipe } from '@angular/common'
import { VrijSlotZonderKamerFilter } from '@shared/types/colon/dto/vrij-slot-zonder-kamer-filter'
import { PagineringDto } from '@shared/types/paginering'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { SorteerRichting } from '@shared/types/sorteer-richting'
import { take } from 'rxjs'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { MatSort, MatSortHeader, Sort } from '@angular/material/sort'

@Component({
  selector: 'app-colon-client-afspraak-zoeken-tabel',
  imports: [
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
    DsButtonComponent,
    DsPaginatorComponent,
    MatSort,
    MatSortHeader,
  ],
  templateUrl: './colon-client-afspraak-zoeken-tabel.component.html',
  styleUrl: './colon-client-afspraak-zoeken-tabel.component.scss',
})
export class ColonClientAfspraakZoekenTabelComponent implements AfterViewInit {
  private readonly afspraakService = inject(ColonIntakeafspraakService)
  private readonly sort = viewChild(MatSort)
  private readonly destroyRef = inject(DestroyRef)

  afspraakslots: WritableSignal<VrijSlotZonderKamerDto[]> = signal([])
  query = input<VrijSlotZonderKamerFilter | null>(null)
  selecteerSlot = output<VrijSlotZonderKamerDto>()

  protected readonly displayedColumns = ['datum', 'tijd', 'naam', 'plaats', 'actie']
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT
  protected paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 20, totaal: 0 })
  protected sortering = signal<SorteerParameterDto>({ veld: 'vanaf', richting: SorteerRichting.ASC })

  constructor() {
    effect(() => {
      if (this.query()) {
        untracked(() => this.getAfspraakslots())
      }
    })
  }

  getAfspraakslots() {
    this.afspraakService
      .zoekAfspraakslots(this.query()!, this.paginering(), this.sortering())
      .pipe(take(1))
      .subscribe((response) => {
        this.paginering.set(response.paginering)
        this.afspraakslots.set(response.data)
      })
  }

  ngAfterViewInit(): void {
    this.sort()
      ?.sortChange.pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe((sortEvent: Sort) => {
        if (!sortEvent.direction) {
          this.sortering.set({ veld: 'vanaf', richting: SorteerRichting.ASC })
        } else {
          this.sortering.set({
            veld: sortEvent.active,
            richting: sortEvent.direction === 'asc' ? SorteerRichting.ASC : SorteerRichting.DESC,
          })
        }

        this.paginering.update((paginering) => ({ ...paginering, paginaNummer: 1 }))

        this.getAfspraakslots()
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

    this.getAfspraakslots()
  }
}
