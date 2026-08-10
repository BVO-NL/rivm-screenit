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
import { Routes } from '@angular/router'
import { loggedInGuard } from './guards/logged-in/logged-in.guard'
import { scopeGuard } from './guards/scope/scope.guard'
import { AuthenticationScope } from './models/AuthenticationScope'

export const routes: Routes = [
  {
    path: '',
    redirectTo: 'labformulieren',
    pathMatch: 'full',
  },
  {
    path: 'labformulieren',
    loadComponent: () => import('./pages/labformulieren-page/labformulieren-page.component').then((m) => m.LabformulierenPageComponent),
    canActivate: [loggedInGuard],
  },
  {
    path: 'login',
    loadComponent: () => import('./pages/authenticatie/login-page/login-page.component').then((m) => m.LoginPageComponent),
  },
  {
    path: 'registreren',
    children: [
      {
        path: '',
        loadComponent: () => import('./pages/authenticatie/registreren-page/registreren-page.component').then((m) => m.RegistrerenPageComponent),
      },
      {
        path: 'voltooien',
        loadComponent: () => import('./pages/authenticatie/registreren-voltooien-page/registreren-voltooien-page.component').then((m) => m.RegistrerenVoltooienPageComponent),
        canActivate: [loggedInGuard, scopeGuard],
        data: {
          redirect: '/registreren',
          scope: AuthenticationScope.REGISTREREN,
        },
      },
    ],
  },
  {
    path: 'wachtwoordvergeten',
    children: [
      {
        path: '',
        loadComponent: () => import('./pages/authenticatie/wachtwoord-vergeten-page/wachtwoord-vergeten-page.component').then((m) => m.WachtwoordVergetenPageComponent),
      },
      {
        path: 'registreren',
        loadComponent: () =>
          import('./pages/authenticatie/wachtwoord-vergeten-registreren-page/wachtwoord-vergeten-registreren-page.component').then(
            (m) => m.WachtwoordVergetenRegistrerenPageComponent,
          ),
      },
      {
        path: 'voltooien',
        loadComponent: () =>
          import('./pages/authenticatie/wachtwoord-vergeten-voltooien-page/wachtwoord-vergeten-voltooien-page.component').then((m) => m.WachtwoordVergetenVoltooienPageComponent),
        canActivate: [scopeGuard],
        data: {
          scope: AuthenticationScope.WACHTWOORDVERGETEN,
          redirect: '/login',
        },
      },
    ],
  },
  {
    path: 'verrichtingen',
    loadComponent: () => import('./pages/verrichtingen-page/verrichtingen-page.component').then((m) => m.VerrichtingenPageComponent),
    canMatch: [loggedInGuard],
  },
  {
    path: 'betalingen',
    loadComponent: () => import('./pages/betalingen-page/betalingen-page.component').then((m) => m.BetalingenPageComponent),
    canMatch: [loggedInGuard],
  },
  {
    path: 'gegevens',
    loadComponent: () => import('./pages/gegevens-page/gegevens-page.component').then((m) => m.GegevensPageComponent),
    canMatch: [loggedInGuard],
  },
  {
    path: 'overeenkomst',
    loadComponent: () => import('./pages/gegevens-page/gegevens-page.component').then((m) => m.GegevensPageComponent),
    canMatch: [loggedInGuard],
  },
]
