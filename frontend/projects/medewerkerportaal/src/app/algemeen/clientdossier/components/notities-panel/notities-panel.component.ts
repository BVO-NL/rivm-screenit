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
import {
  DsButtonComponent,
  DsCardComponent,
  DsCardHeaderContentDirective,
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
import { NL_DATE_TIME_FORMAT, NL_TIMEZONE } from '@shared/constants'
import { DatePipe } from '@angular/common'
import { Dialog } from '@angular/cdk/dialog'
import { NotitieDialogComponent } from '@algemeen/clientdossier/components/notitie-dialog/notitie-dialog.component'
import { ClientContactDto } from '@shared/types/algemeen/dto/client-contact.dto'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { Required } from '@shared/types/autorisatie/required'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { ClientService } from '@algemeen/services/client/client.service'
import { faAdd } from '@fortawesome/pro-light-svg-icons'
import { faFile } from '@fortawesome/pro-solid-svg-icons'

import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { filter, take } from 'rxjs'
import { NaamPipe, NaamTransform } from '@shared/pipes/naam/naam.pipe'
import { AbbreviatePipe } from '@shared/pipes/abbreviate/abbreviate.pipe'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'

@Component({
  selector: 'app-notities-panel',
  imports: [
    DsCardComponent,
    DsButtonComponent,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderCellDef,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    DatePipe,
    MatSort,
    MatSortHeader,
    DsCardHeaderContentDirective,
    DsIconComponent,
    AutorisatieDirective,
    NaamPipe,
    AbbreviatePipe,
    EmptyStatePanelComponent,
  ],
  templateUrl: './notities-panel.component.html',
  styleUrl: './notities-panel.component.scss',
})
export class NotitiesPanelComponent {
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  protected readonly NL_TIMEZONE = NL_TIMEZONE
  protected readonly NaamTransform = NaamTransform
  protected readonly faFile = faFile
  private readonly dialog = inject(Dialog)
  private readonly sort = viewChild(MatSort)

  private readonly autorisatieService = inject(AutorisatieService)
  private readonly contactActieService = inject(ClientContactService)
  private readonly clientService = inject(ClientService)

  protected readonly displayedColumns = ['datumTijd', 'notitie', 'medewerker', 'actie']
  protected readonly notities: WritableSignal<ClientContactDto[]> = signal([])
  protected dataSource = computed(() => {
    const source = new DsTableDataSource(this.notities())
    source.sort = this.sort()
    return source
  })
  protected readonly addIcon = faAdd
  protected readonly toevoegenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
    actie: Actie.TOEVOEGEN,
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    required: Required.ALL,
  }
  protected readonly bewerkenToegestaan = this.autorisatieService.isToegestaan({
    recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
    actie: Actie.AANPASSEN,
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    required: Required.ALL,
  })

  constructor() {
    this.getContacten()
  }

  private getContacten() {
    this.contactActieService
      .getContacten(this.clientService.clientId(), ClientContactActieType.GEEN)
      .pipe(take(1))
      .subscribe((contacten) => this.notities.set(contacten))
  }

  openNotitie(contact?: ClientContactDto) {
    this.dialog
      .open(NotitieDialogComponent, { data: { contact, readonly: !this.bewerkenToegestaan, toonHistorieGegevens: false } })
      .closed.pipe(
        take(1),
        filter((moetVerversen: unknown) => moetVerversen === true),
      )
      .subscribe(() => {
        this.getContacten()
      })
  }
}
