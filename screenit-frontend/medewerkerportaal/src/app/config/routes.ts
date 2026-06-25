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
import { Routes } from '@angular/router'

export const routes: Routes = [
  {
    path: '',
    loadChildren: () => import('../algemeen/algemeen.routes').then((m) => m.algemeenRoutes),
  },
  {
    path: 'borstkanker',
    loadChildren: () => import('../mamma/mamma.routes').then((m) => m.mammaRoutes),
  },
  {
    path: 'baarmoederhalskanker',
    loadChildren: () => import('../cervix/cervix.routes').then((m) => m.cervixRoutes),
  },
  {
    path: 'darmkanker',
    loadChildren: () => import('../colon/colon.routes').then((m) => m.colonRoutes),
  },
  {
    path: 'extra-beveiligde-omgeving',
    children: [
      {
        path: 'client-zoeken',
        loadComponent: () =>
          import('../algemeen/extra-beveiligde-omgeving/extra-beveiligde-omgeving-client-zoeken-page/extra-beveiligde-omgeving-client-zoeken-page.component').then(
            (m) => m.ExtraBeveiligdeOmgevingClientZoekenPageComponent,
          ),
      },
      {
        path: 'keuze-herstellen',
        loadComponent: () =>
          import('../algemeen/extra-beveiligde-omgeving/extra-beveiligde-omgeving-keuze-herstellen-page/extra-beveiligde-omgeving-keuze-herstellen-page.component').then(
            (m) => m.ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent,
          ),
      },
    ],
  },
  {
    path: 'handleidingen',
    loadComponent: () => import('../algemeen/handleidingen-overzicht-page/handleidingen-overzicht-page.component').then((m) => m.HandleidingenOverzichtPageComponent),
  },
]
