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
import { ClrAlertModule } from '@clr/angular'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { Signaleringstermijn } from '@shared/types/signaleringstermijn'

@Component({
  selector: 'app-signaleringstermijn-melding',
  imports: [ClrAlertModule],
  template: ` @if (signaleringstermijn()?.heeftGeenCapaciteitBinnenSignaleringsTermijn) {
    <clr-alert clrAlertType="warning" aria-live="assertive">
      <clr-alert-item>
        <span class="alert-text">
          De afspraakslots voor de periode {{ signaleringstermijn()?.tekst }} zijn nog niet ingevoerd. Om cliënten uit te kunnen nodigen is het noodzakelijk dat deze slots voor
          {{ signaleringstermijn()?.signaleringsTermijnDeadline }} in ScreenIT worden ingevoerd.
        </span>
      </clr-alert-item>
    </clr-alert>
  }`,
})
export class SignaleringstermijnMeldingComponent implements OnInit {
  private roosterService: RoosterService = inject(RoosterService)
  signaleringstermijn: Signal<Signaleringstermijn | null> = this.roosterService.signaleringstermijn

  ngOnInit() {
    this.roosterService.fetchSignaleringstermijn().subscribe()
  }
}
