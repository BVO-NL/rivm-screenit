/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Component, Input } from '@angular/core'

import { ColonBulkAanmakenException } from '@shared/types/colon/colon-bulk-aanmaken-exception'

@Component({
  selector: 'app-bulk-aanmaken-resultaten-tabel',
  imports: [],
  template: `
    <table aria-label="bulk aanmaken resultaten" class="table table-compact table-noborder">
      <thead>
        <tr>
          <th class="clr-text-left datum">Datum</th>
          <th class="clr-text-left tijd">Tijd</th>
          <th class="clr-text-left">Reden</th>
        </tr>
      </thead>
      <tbody>
        @for (exception of exceptions; track exception) {
          <tr>
            <td class="clr-text-left">{{ exception.tijdslot.datum }}</td>
            <td class="clr-text-left">{{ exception.tijdslot.vanaf }} - {{ exception.tijdslot.tot }}</td>
            <td class="clr-text-left">{{ exception.exception }}</td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: [
    `
      :host {
        --clr-table-margin: 0.6rem 0 0 0;
      }

      .datum {
        width: 60px;
      }

      .tijd {
        width: 70px;
      }
    `,
  ],
})
export class BulkAanmakenResultatenTabelComponent {
  @Input({ required: true }) exceptions: ColonBulkAanmakenException[] = []
}
