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
import { afterNextRender, ChangeDetectorRef, Component, inject } from '@angular/core'
import { DsButtonComponent, DsIconComponent, DsLazyTabbedContentDirective, DsPageHeaderComponent, DsTabComponent, DsTabGroupComponent } from '@topicus-rgp-ds/web'
import { faSearch } from '@fortawesome/pro-light-svg-icons'
import { ClientgegevensTabComponent } from '@/algemeen/clientdossier/clientgegevens-tab/clientgegevens-tab.component'
import { ClientService } from '@/algemeen/services/client/client.service'
import { Router } from '@angular/router'

@Component({
  selector: 'app-client-dossier-page',
  imports: [DsButtonComponent, DsIconComponent, DsTabComponent, ClientgegevensTabComponent, DsLazyTabbedContentDirective, DsTabGroupComponent, DsPageHeaderComponent],
  templateUrl: './clientdossier-page.component.html',
  styleUrl: './clientdossier-page.component.scss',
})
export class ClientdossierPageComponent {
  private readonly changeDetection = inject(ChangeDetectorRef)
  protected readonly client = inject(ClientService).select('client')
  private readonly router = inject(Router)

  constructor() {
    afterNextRender(() => {
      requestAnimationFrame(() => {
        this.changeDetection.markForCheck()
      })
    })
  }

  naarClientZoeken() {
    this.router.navigateByUrl('/client/zoeken')
  }

  protected readonly faSearch = faSearch
}
