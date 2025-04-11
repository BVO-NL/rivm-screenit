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
import { Component } from '@angular/core'

@Component({
  selector: 'app-rooster-legenda',
  imports: [],
  template: ` <div class="legenda tooltip">
    <div class="legenda-item tooltip">
      <div class="legenda-item-square legenda-item-square-vrij"></div>
      <span>Vrij</span>
      <div class="tooltip-content">Vrij te verplaatsen</div>
    </div>
    <div class="legenda-item tooltip">
      <div class=" legenda-item-square legenda-item-square-capaciteit"></div>
      <span>Capaciteit</span>
      <div class="tooltip-content">Gebruikt voor capaciteitsberekening</div>
    </div>
    <div class="legenda-item tooltip">
      <div class=" legenda-item-square legenda-item-square-intake"></div>
      <span>Intake</span>
      <div class="tooltip-content">Intake ingepland</div>
    </div>
    <div class="legenda-item tooltip">
      <div class=" legenda-item-square legenda-item-square-blokkade"></div>
      <span>Geblokkeerd</span>
      <div class="tooltip-content">Geblokkeerd door een blokkade</div>
    </div>
    <div class="legenda-item tooltip">
      <div class=" legenda-item-square legenda-item-square-beperking"></div>
      <span>Beperkt planbaar</span>
      <div class="tooltip-content">Weekend, tijd en feestdag beperkt planbaar</div>
    </div>
  </div>`,
  styles: [
    `
      .legenda {
        max-width: 615px;
        display: flex;

        .legenda-item {
          padding: 0.3rem 0.5rem;
          display: flex;
          align-items: center;

          .legenda-item-square {
            width: 20px;
            height: 20px;
            border-radius: 4px;
            margin-right: 8px;

            &-vrij {
              background-color: var(--event-status-free-background-color);
            }
            &-capaciteit {
              background-color: var(--event-status-capacity-background-color);
            }
            &-intake {
              background-color: var(--event-status-intake-background-color);
            }
            &-blokkade {
              background-color: var(--event-status-blocked-background-color);
            }
            &-beperking {
              background-color: var(--event-status-beperking-background-color);
            }
          }
        }
      }
    `,
  ],
})
export class RoosterLegendaComponent {}
