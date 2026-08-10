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
import { Component, effect, inject, Signal } from '@angular/core'
import { RoosterComponent } from '@/colon/colon-rooster-page/components/rooster/rooster.component'
import { ColonService } from '@/colon/services/colon.service'
import { Intakelocatie } from '@shared/types/intakelocatie'
import { DsContextualNotificationsComponent, DsContextualNotificationService } from '@topicus-rgp-ds/web'
import { toSignal } from '@angular/core/rxjs-interop'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { PageComponent } from '@shared/components/page/page.component'
import { take } from 'rxjs'

@Component({
  selector: 'app-colon-rooster-page',
  template: `
    <app-page [titel]="intakelocatie()?.naam ?? ''" class="pagina">
      <div>
        Geprognosticeerde aantal afspraakslots: {{ intakelocatie()?.aantalGeprognosticeerdeAfspraakslots }} / Huidige aantal gemaakte afspraakslots dit jaar:
        {{ intakelocatie()?.huidigAantalAfspraakslots }}
      </div>
      <ds-contextual-notifications context="signaleringstermijn" class="mb-3 mt-3" />
      <app-rooster class="rooster" />
    </app-page>
  `,
  styles: `
    .pagina {
      height: calc(100vh - 168px);
      & > .rooster {
        flex: 1 1 auto;
        min-height: 0;
      }
    }
  `,
  imports: [RoosterComponent, DsContextualNotificationsComponent, PageComponent],
})
export class ColonRoosterPageComponent {
  private readonly colonService: ColonService = inject(ColonService)
  private readonly roosterService: RoosterService = inject(RoosterService)
  private readonly notificationService = inject(DsContextualNotificationService)
  intakelocatie: Signal<Intakelocatie | undefined> = toSignal(this.colonService.fetchIntakelocatie())

  constructor() {
    this.roosterService.fetchSignaleringstermijn().pipe(take(1)).subscribe()

    effect(() => {
      const signaleringstermijn = this.roosterService.signaleringstermijn()
      this.notificationService.removeNotificationByContext('signaleringstermijn')
      if (signaleringstermijn?.heeftGeenCapaciteitBinnenSignaleringsTermijn) {
        this.notificationService.warning(
          'signaleringstermijn',
          {
            message: `De afspraakslots voor de periode ${signaleringstermijn.tekst} zijn nog niet ingevoerd. Om cliënten uit te kunnen nodigen is het noodzakelijk dat deze slots voor ${signaleringstermijn.signaleringsTermijnDeadline} in ScreenIT worden ingevoerd.`,
            closeable: true,
          },
          'page',
        )
      }
    })
  }
}
