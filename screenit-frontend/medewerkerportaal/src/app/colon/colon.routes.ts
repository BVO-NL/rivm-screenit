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

export const colonRoutes: Routes = [
  {
    path: 'rooster',
    loadComponent: () => import('./colon-rooster-page/colon-rooster-page.component').then((m) => m.ColonRoosterPageComponent),
  },
  {
    path: 'feestdagen-beheer',
    loadComponent: () => import('./colon-feestdagen-beheer-page/colon-feestdagen-beheer-page.component').then((m) => m.ColonFeestdagenBeheerPageComponent),
  },
  {
    path: 'weekend-werkdag-beheer',
    loadComponent: () => import('./colon-weekend-werkdag-beperkingen/colon-weekend-werkdag-beperkingen-page.component').then((m) => m.ColonWeekendWerkdagBeperkingenPageComponent),
  },
]
