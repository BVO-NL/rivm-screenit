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
import { ChangeDetectorRef, Component, inject, OnInit } from '@angular/core'
import { DsContextualNotificationsComponent, DsContextualNotificationService } from '@topicus-rgp-ds/web'
import { take } from 'rxjs'
import { ScreeningsorganisatiePanelComponent } from '@/algemeen/clientdossier/clientgegevens-tab/pages/components/screeningsorganisatie-panel/screeningsorganisatie-panel.component'
import { ProjectenPanelComponent } from '@/algemeen/clientdossier/clientgegevens-tab/pages/components/projecten-panel/projecten-panel.component'
import { BvoStatusPanelComponent } from '@/algemeen/clientdossier/clientgegevens-tab/pages/components/bvo-status-panel/bvo-status-panel.component'
import { ClientService } from '@/algemeen/services/client/client.service'
import { AlgemeneBrievenPanelComponent } from '@/algemeen/clientdossier/clientgegevens-tab/pages/components/algemene-brieven-panel/algemene-brieven-panel.component'
import { AdresgegevensPanelComponent } from '@algemeen/clientdossier/clientgegevens-tab/pages/components/adresgegevens-panel/adresgegevens-panel.component'
import { BrpPanelComponent } from '@algemeen/clientdossier/clientgegevens-tab/pages/components/brp-panel/brp-panel.component'
import { ClientInfoPanelComponent } from '@algemeen/clientdossier/clientgegevens-tab/pages/components/client-info-panel/client-info-panel.component'

@Component({
  selector: 'app-overzicht-page',
  imports: [
    BvoStatusPanelComponent,
    ScreeningsorganisatiePanelComponent,
    ProjectenPanelComponent,
    AdresgegevensPanelComponent,
    DsContextualNotificationsComponent,
    AlgemeneBrievenPanelComponent,
    BrpPanelComponent,
    ClientInfoPanelComponent,
  ],
  templateUrl: 'clientdossier-overzicht-page.component.html',
  styleUrl: 'clientdossier-overzicht-page.component.scss',
})
export class ClientdossierOverzichtPageComponent implements OnInit {
  protected static readonly VERTROKKEN_UIT_NEDERLAND_CONTEXT = 'vertrokken-uit-nederland'
  protected toontVertrokkenUitNederlandMelding = false

  private readonly clientService = inject(ClientService)
  private readonly notificationService = inject(DsContextualNotificationService)
  private readonly changeDetectorRef = inject(ChangeDetectorRef)
  private readonly clientUitLocalStorage = this.clientService.select('client')

  ngOnInit() {
    this.toontVertrokkenUitNederlandMelding = false
    this.notificationService.removeNotificationByContext(ClientdossierOverzichtPageComponent.VERTROKKEN_UIT_NEDERLAND_CONTEXT)
    if (this.clientUitLocalStorage()) {
      this.clientService
        .getClientBrpGegevens(this.clientUitLocalStorage().id)
        .pipe(take(1))
        .subscribe((brpGegevens) => {
          if (brpGegevens?.datumVertrokkenUitNederland) {
            this.toontVertrokkenUitNederlandMelding = true
            this.changeDetectorRef.detectChanges()
            this.notificationService.warning(
              ClientdossierOverzichtPageComponent.VERTROKKEN_UIT_NEDERLAND_CONTEXT,
              {
                message: `De cliënt is niet langer woonachtig in Nederland.`,
                closeable: false,
              },
              'page',
            )
          }
        })
    }
  }
}
