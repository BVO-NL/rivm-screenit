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
import { Component, input } from '@angular/core'
import { DsBadgeComponent } from '@topicus-rgp-ds/web'
import { Bevolkingsonderzoek, bevolkingsonderzoekLijst } from '@shared/types/bevolkingsonderzoek'
import { BvoLabelPipe } from '@shared/pipes/bvo-label/bvo-label.pipe'

@Component({
  selector: 'app-bvo-indicator',
  imports: [DsBadgeComponent, BvoLabelPipe],
  template: `
    @for (bvo of bvos(); track bvo) {
      <ds-badge [chip]="true" type="category" [label]="bvo | bvoLabel" class="ds-badge--no-color" />
    }
  `,
})
export class BvoIndicatorComponent {
  bvos = input.required<Bevolkingsonderzoek[]>()
  getLabel(bvo: Bevolkingsonderzoek) {
    return bevolkingsonderzoekLijst.find((b) => b.waarde === bvo)?.naam ?? ''
  }
}
