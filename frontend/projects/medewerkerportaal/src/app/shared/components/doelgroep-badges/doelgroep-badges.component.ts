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
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { Doelgroep, doelgroepLabel } from '@shared/types/algemeen/enum/doelgroep'

@Component({
  selector: 'app-doelgroep-badges',
  imports: [DsBadgeComponent, EnumLabelPipe],
  template: `
    <span class="doelgroepen">
      @for (doelgroep of doelgroepen(); track doelgroep) {
        <ds-badge type="info" [label]="doelgroep | enumLabel: doelgroepLabel" />
      }
    </span>
  `,
  styles: `
    .doelgroepen {
      display: inline-flex;
      flex-wrap: wrap;
      gap: 4px;
    }
  `,
})
export class DoelgroepBadgesComponent {
  readonly doelgroepen = input.required<Doelgroep[]>()
  protected readonly doelgroepLabel = doelgroepLabel
}
