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
  template: `
	  <div class="pagina" [class.pagina--zonder-marge]="zonderMarge()">
		  @if (titel()) {
			  <div class="pagina--header">
				  <h1 class="pagina--titel">{{ titel() }}</h1>
				  <ng-content select="[header-actions]"/>
			  </div>
		  }
		  <div class="pagina--content">
			  <ng-content/>
		  </div>
	  </div> `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      flex: 1;
      min-height: 0;
    }

    .pagina {
      padding: var(--spacer-3);
      display: flex;
      flex-direction: column;
      flex: 1;
      min-height: 0;

      &.pagina--zonder-marge {
        margin: 0;

        &--header {
          margin: var(--spacer-3) var(--spacer-3) 0;
        }
      }

      &--header {
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

      &--content {
        height: 100%;
      }
    }
  `,
})
export class PageComponent {
  titel = input<string | null>(null)
  zonderMarge = input(false)
}
