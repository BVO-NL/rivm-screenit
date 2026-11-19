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
import { Component, computed, DestroyRef, effect, inject, input } from '@angular/core'
import { DsContextualNotificationsComponent, DsContextualNotificationService } from '@topicus-rgp-ds/web'
import { StringsService } from '@shared/strings'
import { splitsPostadres } from '@shared/utils/adres-utils'

@Component({
  selector: 'app-brief-adres-informatiebalk',
  imports: [DsContextualNotificationsComponent],
  template: `<ds-contextual-notifications [context]="context" data-testid="brief-adres-informatiebalk" />`,
})
export class BriefAdresInformatiebalkComponent {
  private readonly notificationService = inject(DsContextualNotificationService)
  private readonly strings = inject(StringsService).getStrings()
  private readonly CONTEXT = 'brief-adres-informatiebalk'

  postadres = input.required<string | null>()

  protected readonly briefAdres = computed(() => splitsPostadres(this.postadres()))
  protected readonly context = this.CONTEXT

  constructor() {
    effect(() => {
      const briefAdres = this.briefAdres()
      this.notificationService.removeNotificationByContext(this.context)
      if (briefAdres) {
        const message = this.strings.algemeen.afspraken.verplaatsenDialog.bevestigingStep.briefAdresMelding(briefAdres.adres, briefAdres.postcode, briefAdres.plaats)
        this.notificationService.info(this.context, { message, closeable: false }, 'page')
      }
    })

    inject(DestroyRef).onDestroy(() => this.notificationService.removeNotificationByContext(this.context))
  }
}
