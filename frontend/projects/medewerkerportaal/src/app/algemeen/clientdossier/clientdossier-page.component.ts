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
import { DsButtonComponent, DsIconComponent, DsLazyTabbedContentDirective, DsPageHeaderComponent, DsTabComponent, DsTabGroupComponent } from '@topicus-rgp-ds/web'
import { faSearch } from '@fortawesome/pro-light-svg-icons'
import { ClientgegevensTabComponent } from '@/algemeen/clientdossier/clientgegevens-tab/clientgegevens-tab.component'
import { ClientService } from '@/algemeen/services/client/client.service'
import { AfsprakenTabComponent } from './afspraken-tab/afspraken-tab.component'
import { ActivatedRoute, Params, Router } from '@angular/router'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { afterNextRender, ChangeDetectorRef, Component, computed, inject, Signal } from '@angular/core'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { MeldingenTabComponent } from '@algemeen/clientdossier/meldingen-tab/meldingen-tab.component'
import { CisHistorieTabComponent } from '@algemeen/clientdossier/cis-historie-tab/cis-historie-tab.component'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter, map } from 'rxjs'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { LoggingService } from '@algemeen/services/logging/logging.service'
import { LogGebeurtenis } from '@shared/types/algemeen/enum/log-gebeurtenis'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'

@Component({
  selector: 'app-client-dossier-page',
  imports: [
    DsButtonComponent,
    DsIconComponent,
    DsTabComponent,
    ClientgegevensTabComponent,
    AfsprakenTabComponent,
    DsLazyTabbedContentDirective,
    DsTabGroupComponent,
    DsPageHeaderComponent,
    MeldingenTabComponent,
    CisHistorieTabComponent,
    AutorisatieDirective,
    CisHistorieTabComponent,
    AfsprakenTabComponent,
  ],
  templateUrl: './clientdossier-page.component.html',
  styleUrl: './clientdossier-page.component.scss',
})
export class ClientdossierPageComponent {
  private readonly changeDetection = inject(ChangeDetectorRef)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly activedRoute = inject(ActivatedRoute)
  private readonly clientService = inject(ClientService)
  private readonly clientContactService = inject(ClientContactService)
  private readonly loggingService = inject(LoggingService)
  private readonly router = inject(Router)

  protected readonly client = inject(ClientService).select('client')
  protected readonly aantalMeldingen: Signal<number> = this.clientContactService.select('aantalContactenMetMelding')
  protected readonly faSearch = faSearch

  protected readonly isAfsprakenTabZichtbaar = computed(
    () =>
      this.clientService.isClientActief() &&
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT],
        actie: Actie.INZIEN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
        required: Required.ANY,
      }),
  )

  protected readonly isCisHistorieTabZichtbaar = computed(
    () =>
      this.clientService.isClientActief() &&
      this.autorisatieService.isToegestaan({
        actie: Actie.INZIEN,
        recht: [Recht.MEDEWERKER_CLIENT_CIS_HISTORIE],
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX],
        level: ToegangLevel.LANDELIJK,
        required: Required.ALL,
      }),
  )

  protected readonly meldingenConstraint = {
    actie: Actie.INZIEN,
    recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ALL,
  }
  protected readonly afsprakenInzienConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    required: Required.ANY,
  }
  protected readonly tabLabels = ['Cliëntgegevens', 'Afspraken', 'Meldingen', 'CIS Historie']
  protected actieveTabIndex = 0

  constructor() {
    const clientId = this.client()?.id
    if (clientId) {
      this.loggingService.logGebeurtenis(LogGebeurtenis.SCREENINGSRONDE_INZIEN, null, clientId)
    }

    afterNextRender(() => {
      requestAnimationFrame(() => {
        this.refresh(this.actieveTabIndex)
      })
    })

    this.clientContactService.getAantalContactenMetMelding(this.clientService.clientId())
    this.activedRoute.queryParams
      .pipe(
        takeUntilDestroyed(),
        map((params: Params) => params['tab']),
        filter((tab: string) => tab !== undefined && this.tabLabels.includes(tab)),
      )
      .subscribe((actieveTab: string) => {
        this.actieveTabIndex = this.tabLabels.findIndex((tab) => tab === actieveTab)
        this.changeDetection.markForCheck()
      })
  }

  naarClientZoeken() {
    this.router.navigateByUrl('/client/zoeken')
  }

  refresh(tabIndex: number) {
    this.actieveTabIndex = tabIndex
    this.changeDetection.markForCheck()
    this.router.navigate([], {
      relativeTo: this.activedRoute,
      queryParams: { tab: this.tabLabels[tabIndex] },
      queryParamsHandling: 'merge',
    })
  }
}
