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
import { AfterViewInit, Component, ElementRef, inject, viewChild } from '@angular/core'
import { ComponentLoaderDirective } from '@shared/directives/component-loader/component-loader.directive'
import { SessionTimeoutService } from '@shared/session-timeout/service/session-timeout.service'
import { ColonRoosterPageComponent } from '@/colon/colon-rooster-page/colon-rooster-page.component'
import { ColonFeestdagenBeheerPageComponent } from '@/colon/colon-feestdagen-beheer-page/colon-feestdagen-beheer-page.component'
import { ColonWeekendWerkdagBeperkingenPageComponent } from '@/colon/colon-weekend-werkdag-beperkingen/colon-weekend-werkdag-beperkingen-page.component'
import { MammaDense2UitwisselingPageComponent } from '@/mamma/mamma-dense2-uitwisseling-page/mamma-dense2-uitwisseling-page.component'
import { ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent } from '@/algemeen/extra-beveiligde-omgeving/extra-beveiligde-omgeving-keuze-herstellen-page/extra-beveiligde-omgeving-keuze-herstellen-page.component'
import { ExtraBeveiligdeOmgevingClientZoekenPageComponent } from '@/algemeen/extra-beveiligde-omgeving/extra-beveiligde-omgeving-client-zoeken-page/extra-beveiligde-omgeving-client-zoeken-page.component'
import { MammaVisitatieOverzichtPageComponent } from '@/mamma/mamma-visitatie-overzicht-page/mamma-visitatie-overzicht-page.component'
import { MammaFotobesprekingOnderzoekenPageComponent } from '@/mamma/mamma-fotobespreking-onderzoeken-page/mamma-fotobespreking-onderzoeken-page.component'

@Component({
  selector: 'app-root',
  template: `<ng-container appComponentLoader></ng-container>`,
  imports: [ComponentLoaderDirective],
})
export class AppComponent implements AfterViewInit {
  componentLoader = viewChild(ComponentLoaderDirective)
  private elementRef: ElementRef = inject(ElementRef)
  private sessionTimeoutService: SessionTimeoutService = inject(SessionTimeoutService)

  get componentName(): string | null {
    return this.elementRef.nativeElement.getAttribute('data-component')
  }

  ngAfterViewInit() {
    this.sessionTimeoutService.startSessionTimer()
    if (!this.componentLoader()) {
      return
    }

    const viewContainerRef = this.componentLoader()!.viewContainerRef
    viewContainerRef.clear()

    switch (this.componentName) {
      case 'colon-rooster':
        viewContainerRef.createComponent<ColonRoosterPageComponent>(ColonRoosterPageComponent)
        break
      case 'feestdagen-beheer':
        viewContainerRef.createComponent<ColonFeestdagenBeheerPageComponent>(ColonFeestdagenBeheerPageComponent)
        break
      case 'weekend-werkdag-beheer':
        viewContainerRef.createComponent<ColonWeekendWerkdagBeperkingenPageComponent>(ColonWeekendWerkdagBeperkingenPageComponent)
        break
      case 'dense2-uitwisseling':
        viewContainerRef.createComponent<MammaDense2UitwisselingPageComponent>(MammaDense2UitwisselingPageComponent)
        break
      case 'extra-beveiligde-omgeving-keuze-herstellen':
        viewContainerRef.createComponent<ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent>(ExtraBeveiligdeOmgevingKeuzeHerstellenPageComponent)
        break
      case 'extra-beveiligde-omgeving-client-zoeken':
        viewContainerRef.createComponent<ExtraBeveiligdeOmgevingClientZoekenPageComponent>(ExtraBeveiligdeOmgevingClientZoekenPageComponent)
        break
      case 'mamma-visitatie-overzicht':
        viewContainerRef.createComponent<MammaVisitatieOverzichtPageComponent>(MammaVisitatieOverzichtPageComponent)
        break
      case 'mamma-fotobespreking-onderzoeken':
        viewContainerRef.createComponent<MammaFotobesprekingOnderzoekenPageComponent>(MammaFotobesprekingOnderzoekenPageComponent)
        break
    }
  }
}
