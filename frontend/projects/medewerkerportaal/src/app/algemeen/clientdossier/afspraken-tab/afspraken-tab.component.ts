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
import { forkJoin, map, switchMap, take } from 'rxjs'
import { ClientPaspoortComponent } from '@algemeen/components/client-paspoort/client-paspoort.component'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'
import { MammaClientafspraakService } from '@mamma/services/mamma-clientafspraak.service'
import { AfspraakPanel } from '@algemeen/clientdossier/components/afspraak-panel/afspraak-panel.component'
import {
  ColonClientAfspraakDialogComponent,
  ColonClientAfspraakDialogMode,
} from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-dialog/colon-client-afspraak-dialog.component'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { ClientAfspraakEnActies } from '@shared/types/algemeen/client-afspraak-en-acties'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'
import { Dialog } from '@angular/cdk/dialog'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { StringsService } from '@shared/strings'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'

@Component({
  selector: 'app-afspraken-tab',
  imports: [AfspraakPanel, ClientPaspoortComponent, ColonClientAfspraakDialogComponent],
  templateUrl: './afspraken-tab.component.html',
  styleUrl: './afspraken-tab.component.scss',
})
export class AfsprakenTabComponent {
  private readonly clientService = inject(ClientService)
  private readonly colonIntakeafspraakService = inject(ColonIntakeafspraakService)
  private readonly mammaClientafspraakService = inject(MammaClientafspraakService)
  private readonly dialog = inject(Dialog)
  private readonly strings = inject(StringsService).getStrings()
  protected readonly Bevolkingsonderzoek = Bevolkingsonderzoek
  protected readonly clientId = this.clientService.clientId()
  protected readonly mammaAfspraakEnActies = signal<ClientAfspraakEnActies<MammaAfspraakDto> | null>(null)
  protected readonly colonAfspraakEnActies = signal<ClientAfspraakEnActies<ColonIntakeafspraakDto> | null>(null)
  protected readonly teVerplaatsenAfspraak: WritableSignal<ClientAfspraakDto | null> = signal(null)
  protected readonly afspraakDialogMode: WritableSignal<ColonClientAfspraakDialogMode> = signal(null)

  protected openAfspraakDialog(dialogMode: ColonClientAfspraakDialogMode, afspraak: ClientAfspraakDto | null) {
    if (afspraak && 'digitaal' in afspraak && afspraak.digitaal) {
      this.dialog
        .open(ConfirmationDialogComponent, {
          data: {
            body: this.strings.algemeen.afspraken.digitaalNaarFysiekDialog.body,
            jaButtonTekst: this.strings.algemeen.afspraken.digitaalNaarFysiekDialog.jaButtonTekst,
            neeButtonTekst: this.strings.algemeen.dialogs.bevestiging.neeButtonTekst,
          },
          hasBackdrop: true,
        })
        .closed.pipe(take(1))
        .subscribe((result) => {
          if (result) {
            this.afspraakDialogMode.set(dialogMode)
            this.teVerplaatsenAfspraak.set(afspraak)
          }
        })
    } else {
      this.afspraakDialogMode.set(dialogMode)
      this.teVerplaatsenAfspraak.set(afspraak)
    }
  }

  protected afspraakDialogSluiten(refreshAfspraak: boolean) {
    this.afspraakDialogMode.set(null)

    if (refreshAfspraak) {
      this.getColonAfspraak()
    }
  }

  constructor() {
    this.getMammaAfspraak()
    this.getColonAfspraak()
  }

  protected getMammaAfspraak() {
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

  protected getColonAfspraak() {
    this.colonIntakeafspraakService
      .getAfspraken(this.clientId)
      .pipe(
        switchMap((afspraken) =>
          this.colonIntakeafspraakService.getAfspraakActies(this.clientId, afspraken.at(0)?.id).pipe(
            map((acties) => ({
              type: Bevolkingsonderzoek.COLON,
              afspraak: afspraken.at(0) ?? null,
              acties,
            })),
          ),
        ),
        take(1),
      )
      .subscribe((afspraakEnActies) => {
        this.colonAfspraakEnActies.set(afspraakEnActies)
      })
  }
}
