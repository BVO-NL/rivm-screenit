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
import {Component, input} from '@angular/core'
import {ColonBulkAanmakenException} from '@shared/types/colon/colon-bulk-aanmaken-exception'

@Component({
  selector: 'app-bulk-aanmaken-resultaten-tabel',
  template: `
    <table class="ds-table">
      <thead>
        <tr class="ds-header-row">
          <th class="ds-header-cell datum-kolom">Datum</th>
          <th class="ds-header-cell datum-kolom">Tijd</th>
          <th class="ds-header-cell">Reden</th>
        </tr>
      </thead>
      <tbody>
        @for (exception of exceptions(); track exception) {
          <tr class="ds-row">
            <td class="ds-cell">{{ exception.tijdslot.datum }}</td>
            <td class="ds-cell">{{ exception.tijdslot.vanaf }} - {{ exception.tijdslot.tot }}</td>
            <td class="ds-cell">{{ exception.exception }}</td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: [
    `
      .datum-kolom {
        width: 100px;
      }
    `,
  ],
})
export class BulkAanmakenResultatenTabelComponent {
  exceptions = input.required<ColonBulkAanmakenException[]>()
}
