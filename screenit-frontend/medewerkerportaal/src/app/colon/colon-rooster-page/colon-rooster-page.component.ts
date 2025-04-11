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
import { Component, inject, OnInit, Signal } from '@angular/core'
import { RoosterComponent } from '@/colon/colon-rooster-page/components/rooster/rooster.component'
import { ColonService } from '@/colon/services/colon.service'
import { Intakelocatie } from '@shared/types/intakelocatie'
import { ClrCommonFormsModule, ClrIconModule } from '@clr/angular'
import { SignaleringstermijnMeldingComponent } from '@/colon/colon-rooster-page/components/signaleringstermijn-melding/signaleringstermijn-melding.component'

@Component({
  selector: 'app-colon-rooster-page',
  template: `
    <div class="card">
      <h3 class="card-header">{{ intakelocatie()?.naam }}</h3>
      <div class="card-block">
        <div class="mb-2">
          Geprognosticeerde aantal afspraakslots: {{ intakelocatie()?.aantalGeprognosticeerdeAfspraakslots }} / Huidige aantal gemaakte afspraakslots dit jaar:
          {{ intakelocatie()?.huidigAantalAfspraakslots }}
        </div>
        <div class="mb-3 mt-3">
          <app-signaleringstermijn-melding />
        </div>
        <app-rooster />
      </div>
    </div>
  `,
  imports: [RoosterComponent, ClrCommonFormsModule, ClrIconModule, SignaleringstermijnMeldingComponent],
})
export class ColonRoosterPageComponent implements OnInit {
  private colonService: ColonService = inject(ColonService)
  intakelocatie: Signal<Intakelocatie | undefined> = this.colonService.intakelocatie

  ngOnInit() {
    this.colonService.fetchIntakelocatie().subscribe()
  }
}
