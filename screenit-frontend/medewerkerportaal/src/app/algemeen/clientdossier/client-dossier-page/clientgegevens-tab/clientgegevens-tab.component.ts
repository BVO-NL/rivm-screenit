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
import {Component, computed, inject, input} from '@angular/core'
import {DsNavigationSidebarComponent} from '@topicus-rgp-ds/web'
import {RouterOutlet} from '@angular/router'
import {ClientDto} from '@shared/types/algemeen/dto/client.dto'
import {getClientDossierNavigationItems} from '@algemeen/clientdossier/navigation-items'
import {ClientService} from '@algemeen/clientdossier/services/client/client.service'
import {toObservable, toSignal} from '@angular/core/rxjs-interop'
import {of, switchMap} from 'rxjs'

@Component({
  selector: 'app-clientgegevens-tab',
  imports: [
    DsNavigationSidebarComponent,
    RouterOutlet,
  ],
  templateUrl: './clientgegevens-tab.component.html',
  styleUrl: './clientgegevens-tab.component.scss',
})
export class ClientgegevensTabComponent {
  private readonly clientService = inject(ClientService)
  client = input<ClientDto>()
  private readonly actieveBvos = toSignal(
    toObservable(this.client).pipe(
      switchMap((client) =>
        client ? this.clientService.getActieveBvos(client.id) : of([]),
      ),
    ),
    {initialValue: []},
  )
  protected readonly clientDossierNavigationItems = computed(() =>
    getClientDossierNavigationItems(this.actieveBvos()),
  )
}
