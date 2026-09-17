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
import { Component, inject, OnInit, signal } from '@angular/core'
import { faPencil } from '@fortawesome/pro-solid-svg-icons'
import { DsButtonComponent, DsCardComponent, DsCardHeaderContentDirective, DsIconComponent } from '@topicus-rgp-ds/web'
import { ClientBrpGegevensDto } from '@shared/types/algemeen/dto/clientbrpgegevens.dto'
import { ClientService } from '@/algemeen/services/client/client.service'
import { take } from 'rxjs'
import { BrpInfoBewerkenModalComponent } from './brp-info-bewerken-modal/brp-info-bewerken-modal.component'
import { Dialog } from '@angular/cdk/dialog'
import { DatePipe } from '@angular/common'
import { EnumNaturalPipe } from '@shared/pipes/enum-natural/enum-natural'
import { BooleanPipe } from '@shared/pipes/boolean/boolean.pipe'
import { NotificationService } from '@shared/services/notification/notification.service'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'

@Component({
  selector: 'app-brp-panel',
  imports: [DsCardComponent, DsButtonComponent, DsCardHeaderContentDirective, DsIconComponent, DatePipe, EnumNaturalPipe, BooleanPipe, AutorisatieDirective, LegeWaardePipe],
  templateUrl: './brp-panel.component.html',
})
export class BrpPanelComponent implements OnInit {
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)
  private readonly notificationService = inject(NotificationService)
  protected clientBrpGegevens = signal<ClientBrpGegevensDto | undefined>(undefined)

  protected readonly inzienConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_GBA_AANVRAGEN],
    actie: Actie.INZIEN,
    required: Required.ANY,
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
  }

  protected readonly wijzigenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_GBA_TIJDELIJK_ADRES],
    actie: Actie.AANPASSEN,
    required: Required.ANY,
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
  }

  ngOnInit() {
    this.haalBrpGegevensOp()
  }

  private haalBrpGegevensOp() {
    this.clientService
      .getClientBrpGegevens(this.clientService.clientId())
      .pipe(take(1))
      .subscribe((brpgegevens) => {
        this.clientBrpGegevens.set(brpgegevens)
      })
  }

  protected openEditModal() {
    const dialogRef = this.dialog.open(BrpInfoBewerkenModalComponent, {
      data: this.clientBrpGegevens(),
    })
    dialogRef.closed.pipe(take(1)).subscribe((result) => {
      if (result === 'opgeslagen') {
        this.notificationService.success('Adresgegevens succesvol opgeslagen')
      } else if (result === 'verwijderd') {
        this.notificationService.success('Adresgegevens succesvol verwijderd')
      }
      this.haalBrpGegevensOp()
    })
  }

  protected readonly faPencil = faPencil
}
