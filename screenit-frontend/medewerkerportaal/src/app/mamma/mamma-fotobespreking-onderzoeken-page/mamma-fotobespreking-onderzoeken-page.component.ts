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
import { Component, inject, signal, WritableSignal } from '@angular/core'
import { PageComponent } from '@shared/components/page/page.component'
import { EnvironmentService } from '@shared/services/environment/environment.service'
import { FotobesprekingService } from '@/mamma/mamma-fotobespreking-onderzoeken-page/services/fotobespreking.service'
import { MammaFotobesprekingDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking.dto'
import { take } from 'rxjs'
import { MammaFotobesprekingFilterComponent } from '@/mamma/mamma-fotobespreking-onderzoeken-page/components/mamma-fotobespreking-filter/mamma-fotobespreking-filter.component'
import { MammaFotobesprekingTabelComponent } from '@/mamma/mamma-fotobespreking-onderzoeken-page/components/mamma-fotobespreking-tabel/mamma-fotobespreking-tabel.component'
import { MammaFotobesprekingOnderzoekFilterDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek-filter.dto'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { WicketUtils } from '@shared/utils/wicket/wicket-utils'
import { PagedResponse } from '@shared/types/paged-response'
import { MammaFotobesprekingOnderzoekDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek.dto'
import { PagineringDto } from '@shared/types/paginering'
import { SorteerParameterDto } from '@shared/types/sort-param'
import { SorteerRichting } from '@shared/types/sorteer-richting'
import { OnderzoekService } from '@/mamma/services/onderzoek/onderzoek.service'
import { NotificationService } from '@shared/services/notification/notification.service'

@Component({
  selector: 'app-mamma-fotobespreking-onderzoeken-page',
  imports: [PageComponent, MammaFotobesprekingFilterComponent, MammaFotobesprekingTabelComponent, DsButtonComponent],
  templateUrl: './mamma-fotobespreking-onderzoeken-page.component.html',
  styleUrl: './mamma-fotobespreking-onderzoeken-page.component.scss',
})
export class MammaFotobesprekingOnderzoekenPageComponent {
  private readonly environmentService = inject(EnvironmentService)
  private readonly fotobesprekingService = inject(FotobesprekingService)
  private readonly onderzoekService = inject(OnderzoekService)
  private readonly notificationService = inject(NotificationService)

  fotobespreking: WritableSignal<MammaFotobesprekingDto | null> = signal(null)
  filter = signal<MammaFotobesprekingOnderzoekFilterDto | null>(null)
  paginaTitel = signal('Fotobespreking bewerken')
  paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 20, totaal: 0 })
  onderzoeken = signal<MammaFotobesprekingOnderzoekDto[]>([])
  sortering = signal<SorteerParameterDto>({
    veld: 'afspraak.uitnodiging.screeningRonde.dossier.client.persoon.achternaam',
    richting: SorteerRichting.ASC,
  })
  geselecteerdeOnderzoeken = signal<MammaFotobesprekingOnderzoekDto[]>([])

  constructor() {
    const fotobesprekingId = this.getFotobesprekingIdFromEnvironment()
    if (fotobesprekingId) {
      this.getFotobespreking(fotobesprekingId)
    }
  }

  private getFotobesprekingIdFromEnvironment(): number | undefined {
    const env = this.environmentService.getEnvironment()
    return env?.fotobesprekingId
  }

  private getFotobespreking(id: number): void {
    this.fotobesprekingService
      .getFotobespreking(id)
      .pipe(take(1))
      .subscribe((response: MammaFotobesprekingDto) => {
        this.fotobespreking.set(response)
        this.paginaTitel.update(() => `Fotobespreking bewerken - ${response.omschrijving}`)
      })
  }

  filterOnderzoeken(filter: MammaFotobesprekingOnderzoekFilterDto) {
    console.log(filter)
    this.filter.set(filter)
    this.getOnderzoeken()
  }

  getOnderzoeken() {
    this.onderzoekService
      .zoekOnderzoeken(this.filter()!, this.sortering(), this.paginering())
      .pipe(take(1))
      .subscribe((response: PagedResponse<MammaFotobesprekingOnderzoekDto[]>) => {
        this.paginering.set(response.paginering)
        this.onderzoeken.set(response.data)
      })
  }

  annuleren() {
    const fotobesprekingId = this.getFotobesprekingIdFromEnvironment()
    if (fotobesprekingId) {
      WicketUtils.toFotobespreking(fotobesprekingId)
    }
  }

  opslaan() {
    const fotobesprekingId = this.getFotobesprekingIdFromEnvironment()
    if (!fotobesprekingId || this.geselecteerdeOnderzoeken().length === 0) {
      return
    }

    this.fotobesprekingService
      .voegOnderzoekenToeAanFotobespreking(
        fotobesprekingId,
        this.geselecteerdeOnderzoeken().map((o) => o.clientId),
      )
      .pipe(take(1))
      .subscribe((meldingen: string[]) => {
        if (meldingen.length > 0) {
          this.notificationService.warning(meldingen.join('\n'))
        } else {
          WicketUtils.toFotobespreking(fotobesprekingId)
        }
      })
  }
}
