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

@Component({
  selector: 'app-page',
  imports: [],
  template: `<div class="pagina">
    <div class="header">
      <h1 class="pagina--titel">{{ titel() }}</h1>
      <ng-content select="[header-actions]" />
    </div>
    <ng-content />
  </div> `,
  styles: `
    .pagina {
      margin: 10px;
      display: flex;
      flex-direction: column;

      .header {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      h1.pagina--titel {
        font-size: 24px;
        font-weight: bold;
        color: var(--ds-color-primary);
        margin: 10px 10px 15px 15px;
        line-height: 40px;
        text-rendering: optimizelegibility;
      }
    }
  `,
})
export class PageComponent {
  titel = input.required<string>()
}
