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
import { Component, computed, input } from '@angular/core'
import { LocatieStatus } from '../../models/LocatieDto'
import { NgClass } from '@angular/common'

@Component({
  selector: 'app-locatie-status',
  imports: [NgClass],
  template: `<div
    class="style"
    [ngClass]="{
      statusActief: status() === LocatieStatus.ACTIEF,
      statusNietGeverifieerd: status() === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD,
      statusVerwijderd: status() !== LocatieStatus.ACTIEF && status() !== LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD,
    }"
  >
    {{ statusText() }}
  </div> `,
  styles: `
    .style {
      height: 20px;
      color: white;
      border-radius: 4px;
      font-size: 0.75rem;
      font-weight: bold;
      text-align: center;
      line-height: 1.5;
      padding: 0 4px;

      &.statusActief {
        background-color: var(--groen);
      }

      &.statusNietGeverifieerd {
        background-color: var(--oranje);
      }

      &.statusVerwijderd {
        background-color: var(--rood);
      }
    }
  `,
})
export class LocatieStatusComponent {
  status = input<LocatieStatus>(LocatieStatus.ACTIEF)
  statusText = computed(() => {
    switch (this.status()) {
      case LocatieStatus.ACTIEF:
        return 'actief'
      case LocatieStatus.INACTIEF:
        return 'verwijderd'
      case LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD:
        return 'verifiëren'
      default:
        return 'onbekend'
    }
  })
  protected readonly LocatieStatus = LocatieStatus
}
