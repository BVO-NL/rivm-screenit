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
import { Component, inject } from '@angular/core'
import { faPencil } from '@fortawesome/pro-solid-svg-icons'
import { DsButtonComponent, DsCardComponent, DsCardHeaderContentDirective, DsIconComponent } from '@topicus-rgp-ds/web'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { ClientService } from '@/algemeen/services/client/client.service'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Dialog } from '@angular/cdk/dialog'
import { filter, switchMap, take } from 'rxjs'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { AdresgegevensBewerkenModalComponent } from './adresgegevens-bewerken-modal/adresgegevens-bewerken-modal.component'

@Component({
  selector: 'app-adresgegevens-panel',
  imports: [DsCardComponent, DsCardHeaderContentDirective, DsIconComponent, DsButtonComponent, AutorisatieDirective, LegeWaardePipe],
  templateUrl: 'adresgegevens-panel.component.html',
})
export class AdresgegevensPanelComponent {
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)
  protected readonly client = this.clientService.select('client')
  protected readonly faPencil = faPencil
  protected readonly adresgegevensWijzigenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_GEGEVENS],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }

  protected openAdresgegevensBewerkenModal() {
    const clientId = this.client()?.id
    if (!clientId) {
      return
    }
    this.clientService
      .getTijdelijkAdres(clientId)
      .pipe(
        take(1),
        switchMap((tijdelijkAdres) => {
          const client = this.client()
          return this.dialog.open(AdresgegevensBewerkenModalComponent, {
            data: {
              ...tijdelijkAdres,
              clientId,
              huidigAdres: {
                volledigeAdres: client?.volledigeAdres ?? '',
                postcode: client?.postcode ?? '',
                plaats: client?.plaats ?? '',
              },
            },
          }).closed
        }),
        filter((opgeslagen) => opgeslagen === true),
        switchMap(() => this.clientService.getClient(clientId).pipe(take(1))),
      )
      .subscribe()
  }
}
