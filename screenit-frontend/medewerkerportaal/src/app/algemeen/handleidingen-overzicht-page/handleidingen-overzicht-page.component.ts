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
import { Component, computed, inject, signal, viewChild } from '@angular/core'
import {
  DsButtonComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsIconComponent,
  DsRowComponent,
  DsTableComponent,
  DsTableDataSource,
} from '@topicus-rgp-ds/web'
import { Handleiding } from '@shared/types/algemeen/handleiding'
import { MatHeaderRowDef, MatRowDef } from '@angular/material/table'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import { faAdd, faDownload, faPen, faTrash } from '@fortawesome/pro-light-svg-icons'

import { filter, switchMap, take } from 'rxjs'
import { HandleidingenService } from '@/algemeen/handleidingen-overzicht-page/services/handleidingen.service'
import { Dialog } from '@angular/cdk/dialog'
import { HandleidingAanmakenDialogComponent } from '@/algemeen/handleidingen-overzicht-page/handleiding-aanmaken-dialog/handleiding-aanmaken-dialog.component'
import { DatePipe } from '@angular/common'
import { HandleidingBewerkenDialogComponent } from '@/algemeen/handleidingen-overzicht-page/handleiding-bewerken-dialog/handleiding-bewerken-dialog.component'
import { NL_DATE_TIME_FORMAT } from '@shared/constants'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { NotificationService } from '@shared/services/notification/notification.service'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { Actie } from '@shared/types/autorisatie/actie'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Required } from '@shared/types/autorisatie/required'
import { HttpErrorResponse } from '@angular/common/http'
import { PageComponent } from '@shared/components/page/page.component'

@Component({
  selector: 'app-handleidingen-overzicht-page',
  imports: [
    DsTableComponent,
    DsHeaderCell,
    DsColumnDef,
    DsCell,
    DsCellDef,
    DsHeaderRowComponent,
    DsRowComponent,
    MatHeaderRowDef,
    MatRowDef,
    MatSort,
    DsHeaderCellDef,
    AutorisatieDirective,
    DsButtonComponent,
    DsIconComponent,
    MatSortHeader,
    DatePipe,
    PageComponent,
  ],
  templateUrl: './handleidingen-overzicht-page.component.html',
})
export class HandleidingenOverzichtPageComponent {
  private readonly handleidingenService: HandleidingenService = inject(HandleidingenService)
  private readonly dialogService: Dialog = inject(Dialog)
  private readonly notificationService: NotificationService = inject(NotificationService)
  private readonly autorisatieService: AutorisatieService = inject(AutorisatieService)

  sort = viewChild(MatSort)

  handleidingen = signal<Handleiding[]>([])

  dataSource = computed(() => {
    const source = new DsTableDataSource(this.handleidingen())
    source.sort = this.sort()
    return source
  })

  columnsToDisplay = computed(() => {
    const columns = ['bestandsnaam']
    if (this.autorisatieService.isToegestaan(this.bewerkenConstraint)) {
      columns.push('bewerkingsdatum', 'laatstGewijzigdDoor')
    }
    columns.push('acties')
    return columns
  })

  readonly faAddIcon: IconDefinition = faAdd
  readonly faDownloadIcon: IconDefinition = faDownload
  readonly faPenIcon: IconDefinition = faPen
  readonly faTrashIcon: IconDefinition = faTrash

  readonly inzienConstraint: SecurityConstraint = {
    recht: ['HANDLEIDINGEN'],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }

  readonly toevoegenConstraint: SecurityConstraint = {
    recht: ['HANDLEIDINGEN'],
    actie: Actie.TOEVOEGEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }

  readonly bewerkenConstraint: SecurityConstraint = {
    recht: ['HANDLEIDINGEN'],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }

  readonly verwijderenConstraint: SecurityConstraint = {
    recht: ['HANDLEIDINGEN'],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }

  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT

  constructor() {
    this.getHandleidingen()
  }

  getHandleidingen() {
    this.handleidingenService
      .getHandleidingen()
      .pipe(take(1))
      .subscribe((handleidingen: Handleiding[]) => {
        this.handleidingen.set(handleidingen)
      })
  }

  openHandleidingDialog() {
    const dialogRef = this.dialogService.open(HandleidingAanmakenDialogComponent, {})
    dialogRef.closed.pipe(take(1)).subscribe((resultaat) => {
      if (resultaat) {
        this.getHandleidingen()
      }
    })
  }

  openHandleidingBewerkenDialog(id: number, bestandsnaam: string) {
    const dialogRef = this.dialogService.open(HandleidingBewerkenDialogComponent, { data: { id, bestandsnaam } })
    dialogRef.closed.pipe(take(1)).subscribe((resultaat) => {
      if (resultaat) {
        this.getHandleidingen()
      }
    })
  }

  downloadHandleiding(id: number, bestandsnaam: string) {
    this.handleidingenService
      .downloadHandleiding(id, bestandsnaam)
      .pipe(take(1))
      .subscribe(() => this.notificationService.success(`${bestandsnaam} is gedownload`))
  }

  openHandleidingVerwijderenDialog(id: number, bestandsnaam: string) {
    this.dialogService
      .open(ConfirmationDialogComponent, {
        data: { title: 'Handleiding verwijderen', body: `Weet u zeker dat u '${bestandsnaam}' wilt verwijderen?` },
      })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.handleidingenService.verwijderHandleiding(id)),
      )
      .subscribe({
        next: () => {
          this.notificationService.success(`'${bestandsnaam}' is succesvol verwijderd.`)
          this.getHandleidingen()
        },
        error: (error: HttpErrorResponse) => {
          const foutmelding = error?.error?.foutmelding ?? 'Er is een fout opgetreden bij het verwijderen van de handleiding'
          this.notificationService.error(foutmelding)
          this.getHandleidingen()
        },
      })
  }
}
