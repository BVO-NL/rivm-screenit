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
import { Component, computed, inject, OnInit, signal } from '@angular/core'
import { DsBadgeComponent, DsButtonComponent, DsCardComponent, DsCardHeaderBadgeDirective, DsCardHeaderContentDirective, DsIconComponent } from '@topicus-rgp-ds/web'
import { DatePipe } from '@angular/common'
import { faPencil } from '@fortawesome/pro-solid-svg-icons'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { geslachtLabel } from '@shared/types/algemeen/enum/geslacht'
import { take } from 'rxjs'
import { Dialog } from '@angular/cdk/dialog'
import { ClientInfoBewerkenModalComponent } from '@/algemeen/clientdossier/clientgegevens-tab/pages/components/client-info-panel/client-info-bewerken-modal/client-info-bewerken-modal.component'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { Doelgroep, doelgroepLabel } from '@shared/types/algemeen/enum/doelgroep'
import { NL_DATE_FORMAT } from '@shared/constants'
import { differenceInYears } from 'date-fns'
import { aanspreekvormLabel } from '@shared/types/algemeen/enum/aanspreekvorm'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { NaamUtils } from '@shared/utils/naam-utils'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { GeslachtIcoonPipe } from '@shared/pipes/geslacht-icoon/geslacht-icoon.pipe'
import { EnumNaturalPipe } from '@shared/pipes/enum-natural/enum-natural'

@Component({
  selector: 'app-client-info-panel',
  imports: [
    DsCardComponent,
    DsCardHeaderBadgeDirective,
    DsCardHeaderContentDirective,
    DsBadgeComponent,
    DsButtonComponent,
    DsIconComponent,
    DatePipe,
    EnumLabelPipe,
    AutorisatieDirective,
    GeslachtIcoonPipe,
    EnumNaturalPipe,
  ],
  templateUrl: './client-info-panel.component.html',
  styleUrl: './client-info-panel.component.scss',
})
export class ClientInfoPanelComponent implements OnInit {
  protected readonly faPencil = faPencil
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly doelgroepLabel = doelgroepLabel
  protected readonly geslachtLabel = geslachtLabel
  protected readonly aanspreekvormLabel = aanspreekvormLabel
  protected readonly bewerkenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_GEGEVENS],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  protected readonly genderIdentiteitConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_TOON_GENDERINDETITEIT],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)
  protected readonly clientUitLocalStorage = this.clientService.select('client')
  protected clientContactgegevens = signal<ClientContactgegevensDto | undefined>(undefined)
  protected readonly heeftDubbeltijd = computed(() => {
    return this.clientContactgegevens()?.doelgroepen?.some((d) => d === Doelgroep.DUBBELE_TIJD) ?? false
  })
  protected readonly leeftijd = computed(() => {
    const geboortedatum = this.clientContactgegevens()?.geboortedatum
    return geboortedatum ? differenceInYears(new Date(), new Date(geboortedatum)) : undefined
  })

  ngOnInit() {
    this.haalContactgegevensOp()
  }

  openContactgegevensBewerkenPopup() {
    const contactgegevens = this.clientContactgegevens()
    if (!contactgegevens) {
      return
    }

    const dialogRef = this.dialog.open(ClientInfoBewerkenModalComponent, {
      data: contactgegevens,
    })
    dialogRef.closed.pipe(take(1)).subscribe(() => {
      this.haalContactgegevensOp()
    })
  }

  private haalContactgegevensOp() {
    const clientId = this.clientService.clientId()
    if (!clientId) {
      return
    }

    this.clientService
      .getClientContactgegevens(clientId)
      .pipe(take(1))
      .subscribe((contactgegevens) => {
        this.clientContactgegevens.set(contactgegevens)
      })
  }

  protected clientContactgegevensTitel(): string {
    const client = this.clientContactgegevens()
    if (client) {
      return NaamUtils.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)
    }
    return ''
  }
}
