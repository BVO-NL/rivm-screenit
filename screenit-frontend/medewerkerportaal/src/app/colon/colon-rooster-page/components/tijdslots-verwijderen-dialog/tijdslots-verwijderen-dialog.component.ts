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
import { Component, inject, signal } from '@angular/core'
import { DatePipe, LowerCasePipe } from '@angular/common'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { ColonTijdslotFilter } from '@shared/types/colon/colon-tijdslot-filter'
import { TijdslotsVerwijderenBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-bevestigings-popup/tijdslots-verwijderen-bevestigings-popup.component'
import { catchError, EMPTY, switchMap, take, throwError } from 'rxjs'
import { HttpErrorResponse } from '@angular/common/http'
import { TijdslotsVerwijderenFilterComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-filter/tijdslots-verwijderen-filter.component'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { SelectOption } from '@shared/types/select-option'
import { NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import {
  DsAbstractSelectableTable,
  DsButtonComponent,
  DsCell,
  DsCellDef,
  DsCheckboxComponent,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsNoDataRow,
  DsPageEvent,
  DsPaginatorComponent,
  DsRowDef,
  DsSelectableRowComponent,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSort } from '@angular/material/sort'
import { NotificationService } from '@shared/services/notification/notification.service'

@Component({
  selector: 'app-tijdslots-verwijderen-dialog',
  imports: [
    BaseDialogComponent,
    ReactiveFormsModule,
    FormsModule,
    TijdslotsVerwijderenFilterComponent,
    DsButtonComponent,
    DsHeaderRowComponent,
    DsHeaderCellDef,
    DsCellDef,
    DsColumnDef,
    DsHeaderRowDef,
    DsRowDef,
    DsHeaderCell,
    DsCell,
    MatSort,
    DsTableComponent,
    DatePipe,
    DsPaginatorComponent,
    DsNoDataRow,
    DsSelectableRowComponent,
    DsCheckboxComponent,
    LowerCasePipe,
  ],
  templateUrl: './tijdslots-verwijderen-dialog.component.html',
})
export class TijdslotsVerwijderenDialogComponent extends DsAbstractSelectableTable<ColonTijdslot> {
  tijdslots = signal<ColonTijdslot[]>([])
  alleTijdslots = signal<ColonTijdslot[]>([])

  typeTijdslot: SelectOption<string> = inject(DIALOG_DATA)
  displayedColumns = ['selection', 'kamer', 'datum', 'starttijd', 'eindtijd']
  pageSize = 500

  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT
  private readonly dialogRef: DialogRef = inject(DialogRef)
  private readonly roosterService: RoosterService = inject(RoosterService)
  private readonly dialogService: Dialog = inject(Dialog)
  private readonly notificationService = inject(NotificationService)

  filter(filter: ColonTijdslotFilter) {
    this.roosterService
      .searchTijdslots(filter, this.typeTijdslot.value)
      .pipe(take(1))
      .subscribe((res) => {
        this.alleTijdslots.set(res)
        this.tijdslots.set(res.slice(0, this.pageSize))
        this.selection.setSelection(...this.tijdslots())
      })
  }

  handlePageChange($event: DsPageEvent): void {
    this.tijdslots.set(this.alleTijdslots().slice($event.pageIndex * $event.pageSize, ($event.pageIndex + 1) * $event.pageSize))
    this.selection.clear()
  }

  cancel() {
    this.dialogRef.close()
  }

  override toggleAll() {
    this.allCheckboxesSelected() ? this.selection.clear() : this.tijdslots().forEach((row) => this.selection.select(row))
  }

  override allCheckboxesSelected() {
    if (this.selection && this.tijdslots()) {
      const numSelected = this.selection.selected.length
      const numRows = this.tijdslots().length
      return numSelected === numRows
    }
    return true
  }

  verwijderen() {
    const selectedTijdslotIds = this.selection.selected.map((slot) => Number(slot.id))
    if (selectedTijdslotIds.length === 0) {
      return
    }

    this.roosterService
      .bulkDeleteTijdslots(selectedTijdslotIds, true, this.typeTijdslot.value)
      .pipe(
        take(1),
        catchError((response: HttpErrorResponse) => {
          if (response.status === 422) {
            return this.dialogService
              .open(TijdslotsVerwijderenBevestigingsPopupComponent, {
                data: {
                  bulkVerwijderenResultaat: response.error,
                  typeTijdslot: this.typeTijdslot,
                },
              })
              .closed.pipe(take(1))
          }

          return throwError(() => response)
        }),
        switchMap((bevestigd) => (bevestigd ? this.roosterService.bulkDeleteTijdslots(selectedTijdslotIds, false, this.typeTijdslot.value) : EMPTY)),
      )
      .subscribe({
        next: () => this.dialogRef.close(),
        error: () => this.notificationService.error('Er is een fout opgetreden bij het verwijderen van de tijdslots. Probeer het opnieuw.'),
      })
  }
}
