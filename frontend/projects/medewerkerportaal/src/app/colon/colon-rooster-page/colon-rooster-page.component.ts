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
import { Component, computed, effect, inject, Signal } from '@angular/core'
import { RoosterComponent } from '@/colon/colon-rooster-page/components/rooster/rooster.component'
import { ColonIntakelocatieService } from '@colon/services/colon-intakelocatie/colon-intakelocatie.service'
import { ColonIntakelocatie } from '@shared/types/colon/colon-intakelocatie'
import { DsContextualNotificationsComponent, DsContextualNotificationService, DsPageHeaderComponent, DsPageSummaryTagComponent } from '@topicus-rgp-ds/web'
import { toSignal } from '@angular/core/rxjs-interop'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { take } from 'rxjs'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-colon-rooster-page',
  template: `
    <ds-page-header [title]="intakelocatie()?.naam ?? ''" [showBackButton]="false">
      <ds-page-summary-tag [label]="subtitel()" />
    </ds-page-header>
    <ds-contextual-notifications context="signaleringstermijn" class="mb-3 mt-3" />
    <app-rooster class="rooster" />
  `,
  styles: `
    .rooster {
      flex: 1 1 auto;
      min-height: 0;
    }
  `,
  imports: [RoosterComponent, DsContextualNotificationsComponent, DsPageHeaderComponent, DsPageSummaryTagComponent],
})
export class ColonRoosterPageComponent {
  private readonly colonService: ColonIntakelocatieService = inject(ColonIntakelocatieService)
  private readonly roosterService: RoosterService = inject(RoosterService)
  private readonly notificationService = inject(DsContextualNotificationService)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly subtitel = computed(
    () =>
      `${this.strings.colon.rooster.geprognosticeerd} ${this.intakelocatie()?.aantalGeprognosticeerdeAfspraakslots ?? 0} ${this.strings.colon.rooster.huidig} ${this.intakelocatie()?.huidigAantalAfspraakslots ?? 0}`,
  )
  intakelocatie: Signal<ColonIntakelocatie | undefined> = toSignal(this.colonService.fetchIntakelocatie())

  constructor() {
    this.roosterService.fetchSignaleringstermijn().pipe(take(1)).subscribe()

    effect(() => {
      const signaleringstermijn = this.roosterService.signaleringstermijn()
      this.notificationService.removeNotificationByContext('signaleringstermijn')
      if (signaleringstermijn?.heeftGeenCapaciteitBinnenSignaleringsTermijn) {
        this.notificationService.warning(
          'signaleringstermijn',
          {
            message: this.strings.colon.rooster.warnings.slotsNietIngevoerd(signaleringstermijn.tekst, signaleringstermijn.signaleringsTermijnDeadline),
            closeable: true,
          },
          'page',
        )
      }
    })
  }
}
