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
import { Component, computed, inject, signal, viewChild, WritableSignal } from '@angular/core'
import { filter, take } from 'rxjs'
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
  DsPaginatorComponent,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
  DsTableDataSource,
} from '@topicus-rgp-ds/web'
import { DatePipe } from '@angular/common'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { NL_DATE_TIME_FORMAT, NL_TIMEZONE } from '@shared/constants'
import { Dialog } from '@angular/cdk/dialog'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Bevolkingsonderzoek, bevolkingsonderzoekAfkortingLabels } from '@shared/types/bevolkingsonderzoek'
import { Required } from '@shared/types/autorisatie/required'
import { NotitieDialogComponent } from '@algemeen/clientdossier/components/notitie-dialog/notitie-dialog.component'
import { ClientContactDto } from '@shared/types/algemeen/dto/client-contact.dto'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { NaamPipe, NaamTransform } from '@shared/pipes/naam/naam.pipe'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { ClientService } from '@algemeen/services/client/client.service'
import { AbbreviatePipe } from '@shared/pipes/abbreviate/abbreviate.pipe'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { clientContactActieLabel } from '@shared/utils/client-contact-actie-utils'
import { naarMatPaginator } from '@shared/utils/ds-paginator-utils'

@Component({
  selector: 'app-historie-contacten-panel',
  imports: [
    DsButtonComponent,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderCellDef,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsPaginatorComponent,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    DatePipe,
    DsCardComponent,
    MatSort,
    MatSortHeader,
    NaamPipe,
    AbbreviatePipe,
    EnumLabelPipe,
  ],
  templateUrl: './historie-contacten-panel.component.html',
  styleUrl: './historie-contacten-panel.component.scss',
})
export class HistorieContactenPanelComponent {
  protected readonly contacten: WritableSignal<ClientContactDto[]> = signal([])
  protected readonly paginaGrootte = 5
  protected readonly dataSource = computed(() => {
    const source = new DsTableDataSource(this.contacten())
    source.sort = this.matSort()
    source.paginator = naarMatPaginator(this.paginator())
    return source
  })
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  protected readonly NL_TIMEZONE = NL_TIMEZONE
  protected readonly NaamTransform = NaamTransform
  protected readonly bevolkingsonderzoekLabels = bevolkingsonderzoekAfkortingLabels
  protected readonly clientContactActieLabel = clientContactActieLabel
  private readonly matSort = viewChild(MatSort)
  private readonly paginator = viewChild(DsPaginatorComponent)
  private readonly dialog = inject(Dialog)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly clientContactService = inject(ClientContactService)
  private readonly clientService = inject(ClientService)
  protected displayedColumns = ['datumTijd', 'acties', 'notitie', 'medewerker', 'bevolkingsonderzoeken']
  protected bewerkenToegestaan = false
  protected inzienToegestaan = false

  constructor() {
    this.laadContacten()

    this.bewerkenToegestaan = this.autorisatieService.isToegestaan({
      recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
      actie: Actie.AANPASSEN,
      level: ToegangLevel.LANDELIJK,
      bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
      required: Required.ALL,
    })
    this.inzienToegestaan = this.autorisatieService.isToegestaan({
      recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
      actie: Actie.INZIEN,
      level: ToegangLevel.LANDELIJK,
      bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
      required: Required.ALL,
    })

    if (this.bewerkenToegestaan || this.inzienToegestaan) {
      this.displayedColumns.push('actie')
    }
  }

  protected isOpmerkingToegestaan(contact: ClientContactDto): boolean {
    return this.bewerkenToegestaan && contact.acties.every((actie) => actie.type !== ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK)
  }

  openNotitie(contact: ClientContactDto) {
    this.dialog
      .open(NotitieDialogComponent, { data: { contact, readonly: !this.isOpmerkingToegestaan(contact), toonHistorieGegevens: true } })
      .closed.pipe(
        take(1),
        filter((result: unknown) => result === true),
      )
      .subscribe(() => this.laadContacten())
  }

  private laadContacten(): void {
    this.clientContactService
      .getContacten(this.clientService.clientId(), ClientContactActieType.GEEN, false)
      .pipe(take(1))
      .subscribe((contacten) => this.contacten.set(contacten))
  }
}
