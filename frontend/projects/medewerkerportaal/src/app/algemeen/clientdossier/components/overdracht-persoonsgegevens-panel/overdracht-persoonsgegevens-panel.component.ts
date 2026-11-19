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
import { Component, computed, inject, viewChild } from '@angular/core'
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
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
  DsTableDataSource,
} from '@topicus-rgp-ds/web'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { ClientService } from '@algemeen/services/client/client.service'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { alleBevolkingsonderzoeken } from '@shared/types/bevolkingsonderzoek'
import { OverdrachtPersoonsgegevensDto } from '@shared/types/algemeen/dto/overdracht-persoonsgegevens.dto'
import { OverdrachtType } from '@shared/types/algemeen/enum/overdracht-type'
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { GebeurtenisBronPipe } from '@shared/pipes/gebeurtenis-bron/gebeurtenis-bron.pipe'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { StringsService } from '@shared/strings'
import { of, switchMap, take } from 'rxjs'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import {
  OverdrachtPersoonsgegevensKeuzesVerwerkenDialogComponent,
  OverdrachtPersoonsgegevensKeuzesVerwerkenDialogData,
} from '@algemeen/clientdossier/acties-tab/components/overdracht-persoonsgegevens-keuzes-verwerken-dialog/overdracht-persoonsgegevens-keuzes-verwerken-dialog.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { Dialog } from '@angular/cdk/dialog'

@Component({
  selector: 'app-overdracht-persoonsgegevens-panel',
  imports: [
    DsCardComponent,
    DsButtonComponent,
    DsTableComponent,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderCellDef,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsRowComponent,
    DsRowDef,
    MatSort,
    MatSortHeader,
    GebeurtenisBronPipe,
    DatumTijdPipe,
  ],
  templateUrl: './overdracht-persoonsgegevens-panel.component.html',
  host: {
    '[class.display-none]': '!isZichtbaar()',
  },
})
export class OverdrachtPersoonsgegevensPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly dialog = inject(Dialog)
  private readonly matSort = viewChild(MatSort)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly displayedColumns = ['status', 'statusDatum', 'actie']

  private readonly isGeautoriseerd = computed(() =>
    this.autorisatieService.isToegestaan({
      recht: [Recht.MEDEWERKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS],
      level: ToegangLevel.LANDELIJK,
      bevolkingsonderzoekScopes: alleBevolkingsonderzoeken,
      actie: Actie.AANPASSEN,
      required: Required.ALL,
    }),
  )
  private readonly overdrachten = toSignal(
    toObservable(this.clientService.select('client')).pipe(switchMap((client) => (this.isGeautoriseerd() ? this.contactActiesService.getInzageOverdrachten(client.id) : of([])))),
    { initialValue: [] as OverdrachtPersoonsgegevensDto[] },
  )
  protected readonly dataSource = computed(() => {
    const source = new DsTableDataSource(this.overdrachten())
    source.sort = this.matSort()
    source.sortingDataAccessor = (overdracht, kolom) => (kolom === 'status' ? this.statusLabel(overdracht) : new Date(overdracht.statusDatum).getTime())
    return source
  })
  protected readonly isZichtbaar = computed(() => this.overdrachten().length > 0 && this.isGeautoriseerd())

  protected statusLabel(overdracht: OverdrachtPersoonsgegevensDto): string {
    const labels = this.strings.algemeen.overdrachtPersoonsgegevens.status
    if (overdracht.type === OverdrachtType.CLIENTPORTAAL) {
      return labels.afgerondViaClientportaal
    }
    switch (overdracht.status) {
      case AanvraagBriefStatus.BRIEF:
        return labels.aangevraagd
      case AanvraagBriefStatus.BRIEF_ONTVANGEN:
        return labels.beschikbaarViaAanvraagformulier
      case AanvraagBriefStatus.VERWERKT:
        return labels.afgerond
      default:
        return labels.onbekend
    }
  }

  protected openOverdrachtPersoonsgegevens(overdracht: OverdrachtPersoonsgegevensDto): void {
    const clientId = this.clientService.clientId()
    this.contactActiesService
      .getInzageVerwerkenGegevens(clientId, overdracht.id)
      .pipe(take(1))
      .subscribe((gegevens) => {
        const data: OverdrachtPersoonsgegevensKeuzesVerwerkenDialogData = { clientId, gegevens }
        this.dialog.open(OverdrachtPersoonsgegevensKeuzesVerwerkenDialogComponent, { data })
      })
  }
}
