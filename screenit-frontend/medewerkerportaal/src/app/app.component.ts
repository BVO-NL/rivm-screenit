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
import { AfterViewInit, Component, ElementRef, inject } from '@angular/core'
import { SessionTimeoutService } from '@shared/session-timeout/service/session-timeout.service'
import { Router, RouterOutlet } from '@angular/router'

@Component({
  selector: 'app-root',
  template: ` <router-outlet></router-outlet>`,
  imports: [RouterOutlet],
})
export class AppComponent implements AfterViewInit {
  private elementRef: ElementRef = inject(ElementRef)
  private sessionTimeoutService: SessionTimeoutService = inject(SessionTimeoutService)
  private readonly router = inject(Router)

  get componentName(): string | null {
    return this.elementRef.nativeElement.getAttribute('data-component')
  }

  ngAfterViewInit() {
    this.sessionTimeoutService.startSessionTimer()
    let route = '/'
    switch (this.componentName) {
      case 'colon-rooster':
        route = '/darmkanker/rooster'
        break
      case 'feestdagen-beheer':
        route = '/darmkanker/feestdagen-beheer'
        break
      case 'weekend-werkdag-beheer':
        route = '/darmkanker/weekend-werkdag-beheer'
        break
      case 'dense2-uitwisseling':
        route = '/borstkanker/dense2-uitwisseling'
        break
      case 'extra-beveiligde-omgeving-keuze-herstellen':
        route = '/extra-beveiligde-omgeving/keuze-herstellen'
        break
      case 'extra-beveiligde-omgeving-client-zoeken':
        route = '/extra-beveiligde-omgeving/client-zoeken'
        break
      case 'mamma-visitatie-overzicht':
        route = '/borstkanker/visitatie'
        break
      case 'mamma-fotobespreking-onderzoeken':
        route = '/borstkanker/fotobespreking/onderzoeken'
        break
      case 'handleidingen-overzicht':
        route = '/handleidingen'
        break
      case 'client-zoeken':
        route = '/client/zoeken'
        break
    }
    this.router.events.subscribe((event) => {
      console.log(event)
    })

    this.router.navigateByUrl(route)
  }
}
