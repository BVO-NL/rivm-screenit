/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Component, ElementRef, inject, Renderer2, ViewChild } from '@angular/core'
import { CommonModule } from '@angular/common'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ClrCheckboxModule, ClrComboboxModule, ClrDatagridModule, ClrDatagridPagination, ClrDatepickerModule, ClrInputModule, ClrRangeModule } from '@clr/angular'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { ReactiveFormsModule } from '@angular/forms'
import { ColonTijdslotFilter } from '@shared/types/colon/colon-tijdslot-filter'
import { TijdslotsVerwijderenBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-bevestigings-popup/tijdslots-verwijderen-bevestigings-popup.component'
import { catchError, filter, switchMap, take, throwError } from 'rxjs'
import { HttpErrorResponse } from '@angular/common/http'
import { TijdslotsVerwijderenFilterComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-filter/tijdslots-verwijderen-filter.component'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { SelectOption } from '@shared/types/select-option'
import { NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'

@Component({
  selector: 'app-tijdslots-verwijderen-dialog',
  imports: [
    CommonModule,
    BaseDialogComponent,
    ClrDatagridModule,
    ClrRangeModule,
    ClrDatepickerModule,
    ReactiveFormsModule,
    ClrInputModule,
    ClrCheckboxModule,
    ClrComboboxModule,
    TijdslotsVerwijderenFilterComponent,
  ],
  templateUrl: './tijdslots-verwijderen-dialog.component.html',
  styles: [
    `
      .checkbox-kolom {
        min-width: 0;
        width: 30px;
      }

      .datagrid {
        overflow: hidden;
      }
    `,
  ],
})
export class TijdslotsVerwijderenDialogComponent {
  private dialogRef: DialogRef = inject(DialogRef)
  private roosterService: RoosterService = inject(RoosterService)
  private dialogService: Dialog = inject(Dialog)
  private renderer: Renderer2 = inject(Renderer2)
  tijdslots: ColonTijdslot[] = []

  @ViewChild('datagridEl', { read: ElementRef }) datagrid: ElementRef | undefined
  @ViewChild(ClrDatagridPagination) pagination: ClrDatagridPagination | undefined
  selectedTijdslots: ColonTijdslot[] = []
  typeTijdslot: SelectOption<string> = inject(DIALOG_DATA)
  huidigePagina = 1

  filter(filter: ColonTijdslotFilter) {
    this.roosterService
      .searchTijdslots(filter, this.typeTijdslot.value)
      .pipe(take(1))
      .subscribe((res) => {
        this.tijdslots = res
        const pageSize = this.pagination?.pageSize ?? 500
        this.selectedTijdslots = res.slice(0, Math.min(res.length, pageSize))
        this.huidigePagina = 1
        this.updateDatagridHeight()
      })
  }

  clearSelection() {
    this.selectedTijdslots = []
  }

  updateDatagridHeight() {
    if (this.datagrid) {
      this.renderer.setStyle(this.datagrid.nativeElement, 'max-height', 'calc(70vh - 210px)')
    }
  }

  cancel() {
    this.dialogRef.close()
  }

  verwijderen() {
    const selectedTijdslotIds = this.selectedTijdslots.map((slot) => Number(slot.id))
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
              .closed.pipe(
                take(1),
                filter((response: unknown) => response === true),
              )
          }
          return throwError(() => response)
        }),
        switchMap(() => this.roosterService.bulkDeleteTijdslots(selectedTijdslotIds, false, this.typeTijdslot.value)),
      )
      .subscribe(() => {
        this.dialogRef.close()
      })
  }

  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT
}
