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
import { Component, computed, effect, input, output, viewChild } from '@angular/core'
import { DatePipe } from '@angular/common'
import {
  DsBadgeComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsNoDataRow,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSort, MatSortHeader, MatSortModule } from '@angular/material/sort'
import { MatTableDataSource } from '@angular/material/table'
import { NL_DATE_FORMAT } from '@shared/constants'
import { ClientDto } from '@shared/types/algemeen/dto/client.dto'
import { VoornaamTussenvoegselAchternaamPipe } from '@shared/pipes/voornaam-tussenvoegsel-achternaam/voornaam-tussenvoegsel-achternaam.pipe'
import { GeslachtIcoonPipe } from '@shared/pipes/geslacht-icoon/geslacht-icoon.pipe'
import { faAngleRight } from '@fortawesome/pro-light-svg-icons'
import { differenceInYears } from 'date-fns'

@Component({
  selector: 'app-client-zoeken-tabel',
  imports: [
    DatePipe,
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsHeaderCellDef,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    MatSort,
    MatSortModule,
    MatSortHeader,
    VoornaamTussenvoegselAchternaamPipe,
    GeslachtIcoonPipe,
    DsIconComponent,
    DsNoDataRow,
    DsBadgeComponent,
  ],
  templateUrl: './client-zoeken-tabel.component.html',
  styleUrl: './client-zoeken-tabel.component.scss',
})
export class ClientZoekenTabelComponent {
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly navigeerIcoon = faAngleRight

  clienten = input.required<ClientDto[]>()

  navigate = output<ClientDto>()

  private readonly matSort = viewChild(MatSort)

  protected readonly dataSource = new MatTableDataSource<ClientDto>([])

  private readonly basisKolommen = ['naam', 'bsn', 'geboortedatum', 'plaats', 'postcode', 'straat', 'navigeer']

  constructor() {
    effect(() => {
      this.dataSource.data = this.clienten()
    })

    effect(() => {
      const sort = this.matSort()
      if (sort) {
        this.dataSource.sort = sort
      }
    })
  }

  protected isEenClientOverleden = computed(() => this.clienten().some((client) => client.overlijdensdatum))

  protected displayedColumns = computed(() => {
    if (this.isEenClientOverleden()) {
      const kolommen = [...this.basisKolommen]
      kolommen.splice(3, 0, 'overlijdensdatum')
      return kolommen
    }
    return this.basisKolommen
  })

  berekenLeeftijd(geboortedatum: Date): number {
    return differenceInYears(new Date(), new Date(geboortedatum))
  }

  navigeerNaarClient(client: ClientDto) {
    this.navigate.emit(client)
  }
}
