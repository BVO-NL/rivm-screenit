/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { Event, NavigationEnd, Router, RouterOutlet } from '@angular/router'
import { HeaderComponent } from './components/header/header.component'
import { LocatieVerificatieMeldingComponent } from './components/locatie-verificatie-melding/locatie-verificatie-melding.component'
import { BuildInfoComponent } from './components/build-info/build-info.component'
import { FaConfig } from '@fortawesome/angular-fontawesome'
import { NotificatieService } from './services/notificatie/notificatie.service'

@Component({
  selector: 'app-root',
  imports: [RouterOutlet, HeaderComponent, LocatieVerificatieMeldingComponent, BuildInfoComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
})
export class AppComponent {
  constructor() {
    const faConfig = inject(FaConfig)
    faConfig.autoAddCss = false

    const router = inject(Router)
    const notificatieService = inject(NotificatieService)
    router.events.subscribe((event: Event) => {
      if (event instanceof NavigationEnd) {
        notificatieService.clear()
      }
    })
  }
}
