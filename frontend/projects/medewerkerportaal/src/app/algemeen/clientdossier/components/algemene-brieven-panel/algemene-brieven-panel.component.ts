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
import { Component, computed, inject, signal } from '@angular/core'
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
} from '@topicus-rgp-ds/web'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ScreeningRondeGebeurtenisDto } from '@shared/types/algemeen/dto/screening-ronde-gebeurtenis.dto'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { typeGebeurtenisLabels } from '@shared/types/algemeen/enum/type-gebeurtenis'
import { GebeurtenisBronPipe } from '@shared/pipes/gebeurtenis-bron/gebeurtenis-bron.pipe'
import { Dialog } from '@angular/cdk/dialog'
import { BriefDialogComponent } from '@algemeen/components/brief-dialog/brief-dialog.component'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { take } from 'rxjs'

@Component({
  selector: 'app-algemene-brieven-panel',
  imports: [
    DsCardComponent,
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
    DsButtonComponent,
    EnumLabelPipe,
    GebeurtenisBronPipe,
    DatumTijdPipe,
  ],
  templateUrl: './algemene-brieven-panel.component.html',
  host: {
    '[class.display-none]': 'isZichtbaar()',
  },
})
export class AlgemeneBrievenPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)
  protected readonly displayedColumns = ['actie', 'naam', 'datum', 'bekijken']
  protected readonly gebeurtenisLabels = typeGebeurtenisLabels

  readonly brieven = signal<ScreeningRondeGebeurtenisDto[]>([])
  isZichtbaar = computed(() => this.brieven().length === 0 && this.clientService.isClientActief())

  constructor() {
    this.laadBrieven()
  }

  openBrief(gebeurtenis: ScreeningRondeGebeurtenisDto) {
    this.dialog
      .open(BriefDialogComponent, { data: gebeurtenis })
      .closed.pipe(take(1))
      .subscribe((briefOpnieuwAangemaakt) => {
        if (briefOpnieuwAangemaakt) {
          this.laadBrieven()
        }
      })
  }

  private laadBrieven() {
    this.clientService
      .getScreeningRondeGebeurtenissen(this.clientService.clientId(), 'algemene-brieven')
      .pipe(take(1))
      .subscribe((brieven) => this.brieven.set(brieven))
  }
}
