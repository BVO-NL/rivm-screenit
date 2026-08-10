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
import { filter, iif, map, switchMap, take } from 'rxjs'
import { ColonFeestdag } from '@shared/types/colon/colon-feestdag'
import { Dialog } from '@angular/cdk/dialog'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ReactiveFormsModule } from '@angular/forms'
import { FeestdagBewerkenDialogComponent } from '@/colon/colon-feestdagen-beheer-page/components/feestdag-bewerken-dialog/feestdag-bewerken-dialog.component'
import { FeestdagenService } from '@/colon/colon-feestdagen-beheer-page/services/feestdagen.service'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@/shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import {
  DsButtonComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
  DsTableDataSource,
} from '@topicus-rgp-ds/web'
import { faAdd, faPenToSquare, faTrashCan } from '@fortawesome/pro-light-svg-icons'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { NL_DATE_FORMAT } from '@shared/constants'
import { DatePipe, TitleCasePipe } from '@angular/common'
import { PageComponent } from '@shared/components/page/page.component'
import { FeestdagFoutmeldingPopupComponent } from '@/colon/colon-feestdagen-beheer-page/components/feestdag-foutmelding-popup/feestdag-foutmelding-popup.component'
import { HttpErrorResponse } from '@angular/common/http'
import { GlobalErrorHandler } from '@shared/services/global-error-handler/global-error-handler'

@Component({
  selector: 'app-colon-feestdagen-beheer-page',
  imports: [
    ReactiveFormsModule,
    AutorisatieDirective,
    DsTableComponent,
    DsColumnDef,
    DsButtonComponent,
    DsIconComponent,
    DsHeaderRowComponent,
    DsRowComponent,
    DsHeaderCellDef,
    DsCellDef,
    DsHeaderRowDef,
    DsRowDef,
    DsHeaderCell,
    DsCell,
    MatSort,
    MatSortHeader,
    DatePipe,
    TitleCasePipe,
    PageComponent,
  ],
  templateUrl: './colon-feestdagen-beheer-page.component.html',
  styles: [
    `
      .header-btn {
        margin-top: 1.2rem;
      }
    `,
  ],
})
export class ColonFeestdagenBeheerPageComponent {
  feestdagen = signal<ColonFeestdag[]>([])
  dataSource = computed(() => {
    const source = new DsTableDataSource(this.feestdagen())
    source.sort = this.sort()
    return source
  })
  private feestdagenService: FeestdagenService = inject(FeestdagenService)
  private dialogService: Dialog = inject(Dialog)
  private readonly errorHandler = inject(GlobalErrorHandler)
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  sort = viewChild(MatSort)

  toevoegenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.TOEVOEGEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  bewerkenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  verwijderenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  displayedColumns: string[] = ['naam', 'datum', 'beperking', 'acties']
  readonly faAddIcon: IconDefinition = faAdd
  readonly faEditIcon: IconDefinition = faPenToSquare
  readonly faDeleteIcon: IconDefinition = faTrashCan

  constructor() {
    this.getFeestdagen()
  }

  getFeestdagen() {
    this.feestdagenService
      .getFeestdagen()
      .pipe(take(1))
      .subscribe((feestdagen: ColonFeestdag[]) => {
        this.feestdagen.set(feestdagen)
      })
  }

  openFeestdagDialog(feestdag?: ColonFeestdag) {
    this.dialogService
      .open(FeestdagBewerkenDialogComponent, { data: feestdag })
      .closed.pipe(
        take(1),
        filter((result: unknown) => !!result),
        map((result: unknown) => result as ColonFeestdag),
        switchMap((feestdag: ColonFeestdag) => iif(() => feestdag.id == null, this.feestdagenService.createFeestdag(feestdag), this.feestdagenService.updateFeestdag(feestdag))),
      )
      .subscribe({
        next: () => this.getFeestdagen(),
        error: (response: HttpErrorResponse) => {
          if (response.status === 422 && response.error?.afspraakslots != null) {
            this.dialogService.open(FeestdagFoutmeldingPopupComponent, { data: response.error })
          } else {
            this.errorHandler.handleError(response.error)
          }
        },
      })
  }

  confirmDeleteFeestdag(feestdag: ColonFeestdag) {
    this.dialogService
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Waarschuwing',
          body: 'Weet u zeker dat u deze feestdag wilt verwijderen?',
        },
        minWidth: '300px',
      })
      .closed.pipe(take(1))
      .subscribe((res: unknown) => {
        if (res === true) {
          this.removeFeestdag(feestdag)
        }
      })
  }

  private removeFeestdag(feestdag: ColonFeestdag) {
    this.feestdagenService
      .removeFeestdag(feestdag)
      .pipe(take(1))
      .subscribe(() => {
        this.getFeestdagen()
      })
  }
}
