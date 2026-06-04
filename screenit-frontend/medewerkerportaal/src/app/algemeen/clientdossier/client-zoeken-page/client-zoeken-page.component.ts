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
import {Component, inject, signal, viewChild} from '@angular/core'
import {PageComponent} from '@shared/components/page/page.component'
import {ClientZoekenTabelComponent} from '@algemeen/clientdossier/client-zoeken-page/components/client-zoeken-tabel/client-zoeken-tabel.component'
import {ClientService} from '@algemeen/clientdossier/services/client/client.service'
import {ClientDto} from '@shared/types/algemeen/dto/client.dto'
import {ClientZoekenFilterDto} from '@shared/types/algemeen/dto/client-zoeken-filter.dto'
import {ClientZoekenFilterComponent} from '@algemeen/clientdossier/client-zoeken-page/components/client-zoeken-filter/client-zoeken-filter.component'
import {take} from 'rxjs'
import {DsIconComponent} from '@topicus-rgp-ds/web'
import {faUser} from '@fortawesome/pro-light-svg-icons'
import {WicketUtils} from '@shared/utils/wicket/wicket-utils'

@Component({
  selector: 'app-client-zoeken-page',
  imports: [
    PageComponent,
    ClientZoekenFilterComponent,
    ClientZoekenTabelComponent,
    DsIconComponent,
  ],
  templateUrl: './client-zoeken-page.component.html',
  styleUrl: './client-zoeken-page.component.scss',
})
export class ClientZoekenPageComponent {
  private readonly clientService = inject(ClientService)
  protected readonly gebruikerIcoon = faUser
  protected readonly filterComponent = viewChild(ClientZoekenFilterComponent)
  protected clienten = signal<ClientDto[]>([])
  protected heeftGezocht = signal(false)

  zoekClienten(filter: ClientZoekenFilterDto) {
    this.clientService.zoekClienten(filter).pipe(take(1)).subscribe((clienten) => {
      this.clienten.set(clienten)
      this.heeftGezocht.set(true)
    })
  }

  navigeerNaarClient(client: ClientDto) {
    this.clientService.set('client', client)
    WicketUtils.toClientgegevens(client.id)
  }
}
