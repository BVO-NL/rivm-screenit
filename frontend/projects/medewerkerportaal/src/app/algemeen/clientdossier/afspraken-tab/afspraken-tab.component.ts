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
import { forkJoin, map, of, switchMap, take } from 'rxjs'
import { ClientPaspoortComponent } from '@algemeen/components/client-paspoort/client-paspoort.component'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'
import { MammaClientafspraakService } from '@mamma/services/mamma-clientafspraak.service'
import { AfspraakPanel } from '@algemeen/clientdossier/components/afspraak-panel/afspraak-panel.component'
import { ColonClientAfspraakVerplaatsenDialogComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-verplaatsen/colon-client-afspraak-verplaatsen-dialog.component'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { ClientAfspraakEnActies } from '@shared/types/algemeen/client-afspraak-en-acties'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'

@Component({
  selector: 'app-afspraken-tab',
  imports: [AfspraakPanel, ClientPaspoortComponent, ColonClientAfspraakVerplaatsenDialogComponent],
  templateUrl: './afspraken-tab.component.html',
  styleUrl: './afspraken-tab.component.scss',
})
export class AfsprakenTabComponent {
  private readonly clientService = inject(ClientService)
  private readonly colonIntakeafspraakService = inject(ColonIntakeafspraakService)
  private readonly mammaClientafspraakService = inject(MammaClientafspraakService)
  protected readonly Bevolkingsonderzoek = Bevolkingsonderzoek
  protected readonly clientId = this.clientService.clientId()
  protected mammaAfspraakEnActies = signal<ClientAfspraakEnActies | null>(null)
  protected colonAfspraakEnActies = signal<ClientAfspraakEnActies | null>(null)
  protected readonly tijdstipWijzigenAfspraak: WritableSignal<ClientAfspraakDto | null> = signal(null)

  protected afspraakTijdstipSluiten() {
    this.tijdstipWijzigenAfspraak.set(null)
    this.getColonAfspraak()
  }

  constructor() {
    this.getMammaAfspraak()
    this.getColonAfspraak()
  }

  private getMammaAfspraak() {
    forkJoin({
      afspraken: this.mammaClientafspraakService.getAfspraken(this.clientId),
      acties: this.mammaClientafspraakService.getAfspraakActies(this.clientId),
    })
      .pipe(
        map(({ afspraken, acties }): ClientAfspraakEnActies<MammaAfspraakDto> => ({
          type: Bevolkingsonderzoek.MAMMA,
          afspraak: afspraken.at(0) ?? null,
          acties,
        })),
        take(1),
      )
      .subscribe((afspraakEnActies) => {
        this.mammaAfspraakEnActies.set(afspraakEnActies)
      })
  }

  private getColonAfspraak() {
    this.colonIntakeafspraakService
      .getAfspraken(this.clientId)
      .pipe(
        switchMap((afspraken) =>
          forkJoin({
            afspraken: of(afspraken),
            acties: this.colonIntakeafspraakService.getAfspraakActies(this.clientId, afspraken.at(0)?.id),
          }),
        ),
        map(({ afspraken, acties }): ClientAfspraakEnActies<ColonIntakeafspraakDto> => ({
          type: Bevolkingsonderzoek.COLON,
          afspraak: afspraken.at(0) ?? null,
          acties,
        })),
        take(1),
      )
      .subscribe((afspraakEnActies) => {
        this.colonAfspraakEnActies.set(afspraakEnActies)
      })
  }
}
