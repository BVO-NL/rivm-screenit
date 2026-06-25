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
import { autorisatieGuard } from '@/autorisatie/guard/autorisatie.guard'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Required } from '@shared/types/autorisatie/required'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'

export const algemeenRoutes: Routes = [
  {
    path: 'client',
    children: [
      {
        path: '',
        redirectTo: 'client/zoeken',
        pathMatch: 'full',
      },
      {
        path: 'zoeken',
        loadComponent: () => import('../algemeen/clientdossier/client-zoeken-page/client-zoeken-page.component').then((c) => c.ClientZoekenPageComponent),
        canActivate: [
          autorisatieGuard({
            recht: [Recht.MEDEWERKER_CLIENT_ZOEKEN],
            actie: Actie.INZIEN,
            level: ToegangLevel.LANDELIJK,
            required: Required.ANY,
            organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE],
            bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
          }),
        ],
      },
      {
        path: 'dossier',
        loadComponent: () => import('../algemeen/clientdossier/client-dossier-page/client-dossier-page.component').then((c) => c.ClientDossierPageComponent),
        canActivate: [
          autorisatieGuard({
            recht: [Recht.MEDEWERKER_CLIENT_GEGEVENS],
            actie: Actie.INZIEN,
            level: ToegangLevel.LANDELIJK,
            required: Required.ANY,
            organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE],
            bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
          }),
        ],
        children: [
          {
            path: '',
            redirectTo: 'overzicht',
            pathMatch: 'full',
          },
          {
            path: 'overzicht',
            loadComponent: () =>
              import('../algemeen/clientdossier/client-dossier-page/clientgegevens-tab/pages/overzicht/overzicht-page.component').then((c) => c.OverzichtPageComponent),
          },
          {
            path: ':bvo',
            loadComponent: () =>
              import('../algemeen/clientdossier/client-dossier-page/clientgegevens-tab/pages/client-dossier-bvo/client-dossier-bvo.component').then(
                (c) => c.ClientDossierBvoComponent,
              ),
          },
        ],
      },
    ],
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
