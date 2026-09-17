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
import { Component } from '@angular/core'
import { NotitiesPanelComponent } from '@algemeen/clientdossier/components/notities-panel/notities-panel.component'
import { HistorieContactenPanelComponent } from '@algemeen/clientdossier/components/historie-contacten-panel/historie-contacten-panel.component'
import { ClientPaspoortComponent } from '@algemeen/components/client-paspoort/client-paspoort.component'

@Component({
  selector: 'app-meldingen-tab',
  imports: [NotitiesPanelComponent, HistorieContactenPanelComponent, ClientPaspoortComponent],
  templateUrl: './meldingen-tab.component.html',
  styles: `
    :host {
      background: var(--stale-background-color);
      width: 100%;
    }
  `,
})
export class MeldingenTabComponent {}
