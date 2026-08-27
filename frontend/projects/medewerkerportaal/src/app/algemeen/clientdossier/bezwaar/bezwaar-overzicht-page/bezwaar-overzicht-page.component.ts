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
import { DsPageHeaderComponent } from '@topicus-rgp-ds/web'
import { Router } from '@angular/router'
import { BezwaarPanelComponent } from '../components/bezwaar-panel/bezwaar-panel.component'
import { BezwaarMomentenPanelComponent } from '../components/bezwaar-momenten-panel/bezwaar-momenten-panel.component'
import { ClientPaspoortComponent } from '@algemeen/components/client-paspoort/client-paspoort.component'
import { ClientService } from '@/algemeen/services/client/client.service'

@Component({
  selector: 'app-bezwaar-overzicht-page',
  imports: [DsPageHeaderComponent, BezwaarPanelComponent, BezwaarMomentenPanelComponent, ClientPaspoortComponent],
  templateUrl: './bezwaar-overzicht-page.component.html',
})
export class BezwaarOverzichtPageComponent {
  private readonly router = inject(Router)
  private readonly clientService = inject(ClientService)
  protected readonly clientId = this.clientService.clientId()

  protected naarClientDossier(): void {
    this.router.navigateByUrl('/client/dossier')
  }
}
