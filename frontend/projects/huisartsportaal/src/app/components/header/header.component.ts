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
import { Component, computed, inject, Signal } from '@angular/core'
import {
  DsButtonComponent,
  DsIconComponent,
  DsIconType,
  DsNavigationTopbarComponent,
  DsNavigationTopbarCustomButtonsDirective,
  DsNavigationTopbarItem,
  DsTopbarNavigationConfigModel,
} from '@topicus-rgp-ds/web'
import { Router, RouterLink } from '@angular/router'
import { faRightFromBracket, faUserEdit } from '@fortawesome/pro-solid-svg-icons'
import { AuthService } from '../../services/auth/auth.service'
import huisartsportaalLogo from './huisartsportaal-logo'
import { Recht } from '../../models/Recht'
import { AuthenticationScope } from '../../models/AuthenticationScope'

interface TopbarAction {
  label: string
  callback?: () => void
  routerLink?: string
  icon: DsIconType
  testId: string
}
@Component({
  selector: 'app-header',
  imports: [DsNavigationTopbarComponent, DsNavigationTopbarCustomButtonsDirective, RouterLink, DsButtonComponent, DsIconComponent],
  templateUrl: './header.component.html',
  styles: `
    .ds-button {
      --ds-text-color: #fff;
      &:hover {
        --ds-text-color: #475055ff;
      }
    }
  `,
})
export class HeaderComponent {
  private readonly authService = inject(AuthService)
  private readonly router = inject(Router)

  topbarNavigationConfig = new DsTopbarNavigationConfigModel({
    base64Logo: huisartsportaalLogo,
  })
  isLoggedIn = computed(() => this.authService.isLoggedIn())
  navigationItems: Signal<DsNavigationTopbarItem[]> = computed(() =>
    this.isLoggedIn() && this.authService.heeftRecht(Recht.ROLE_AANVRAGEN) && this.authService.heeftScope(AuthenticationScope.LOGIN)
      ? [
          {
            label: 'Labformulieren aanvragen',
            routerLink: 'labformulieren',
          },
          {
            label: 'Overzicht verrichtingen',
            routerLink: 'verrichtingen',
          },
          {
            label: 'Overzicht betalingen',
            routerLink: 'betalingen',
          },
        ]
      : [],
  )

  actions: Signal<TopbarAction[]> = computed(() => {
    const actions = []
    if (this.isLoggedIn()) {
      if (this.authService.heeftRecht(Recht.ROLE_AANVRAGEN) && this.authService.heeftScope(AuthenticationScope.LOGIN)) {
        actions.push({
          label: 'Wijzig gegevens',
          routerLink: 'gegevens',
          icon: faUserEdit,
          testId: 'wijzig_gegevens_button',
        })
      }
      actions.push({
        label: 'Uitloggen',
        callback: () => this.logout(),
        icon: faRightFromBracket,
        testId: 'logout_button',
      })
    }
    return actions
  })

  logout() {
    this.authService.logout()
    this.router.navigate(['login'])
  }
}
