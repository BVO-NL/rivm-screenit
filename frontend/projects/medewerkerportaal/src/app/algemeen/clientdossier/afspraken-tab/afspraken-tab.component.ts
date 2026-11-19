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
import { MammaClientafspraakService } from '@mamma/services/mamma-clientafspraak/mamma-clientafspraak.service'
import { MammaDossierService } from '@mamma/services/mamma-dossier/mamma-dossier.service'
import { MammaUitstelService } from '@mamma/services/mamma-uitstel/mamma-uitstel.service'
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
import { MammaUitstelDto } from '@shared/types/mamma/dto/mamma-uitstel.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'
import { Dialog } from '@angular/cdk/dialog'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { StringsService } from '@shared/strings'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { MammaAfspraakPanelComponent } from '@algemeen/clientdossier/afspraken-tab/components/mamma-afspraak-panel/mamma-afspraak-panel.component'
import { MammaDossierDto } from '@shared/types/mamma/dto/mamma-dossier.dto'
import { Deelnamemodus } from '@shared/types/algemeen/enum/deelnamemodus'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { DateService } from '@shared/services/date/date.service'
import { formatDate } from '@shared/utils/date-utils'

interface MammaAfspraakGegevens {
  afspraak: MammaAfspraakDto | null
  uitstel: MammaUitstelDto | null
  acties: AfspraakActie[]
}

@Component({
  selector: 'app-afspraken-tab',
  imports: [AfspraakPanel, ClientPaspoortComponent, ColonClientAfspraakDialogComponent, MammaAfspraakPanelComponent],
  templateUrl: './afspraken-tab.component.html',
  styleUrl: './afspraken-tab.component.scss',
})
export class AfsprakenTabComponent {
  private readonly clientService = inject(ClientService)
  private readonly colonIntakeafspraakService = inject(ColonIntakeafspraakService)
  private readonly mammaClientafspraakService = inject(MammaClientafspraakService)
  private readonly mammaDossierService = inject(MammaDossierService)
  private readonly mammaUitstelService = inject(MammaUitstelService)
  private readonly dialog = inject(Dialog)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly strings = inject(StringsService).getStrings()
  protected readonly Bevolkingsonderzoek = Bevolkingsonderzoek
  protected readonly clientId = this.clientService.clientId()
  protected readonly magColonAfsprakenInzien = this.autorisatieService.isToegestaan({
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    required: Required.ANY,
  })
  protected readonly magMammaAfsprakenInzien = this.autorisatieService.isToegestaan({
    recht: [Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAKEN],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
    required: Required.ANY,
  })
  protected readonly mammaAfspraakGegevens = signal<MammaAfspraakGegevens | null>(null)
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
    if (this.magMammaAfsprakenInzien) {
      this.getMammaAfspraak()
    }
    if (this.magColonAfsprakenInzien) {
      this.getColonAfspraak()
    }
  }

  protected bevestigingsbriefAangevraagd() {
    this.mammaAfspraakGegevens.update((gegevens) =>
      gegevens?.afspraak ? { ...gegevens, afspraak: { ...gegevens.afspraak, bevestigingsbriefAangevraagdOp: formatDate(DateService.nu()) } } : gegevens,
    )
  }

  private getMammaAfspraak() {
    forkJoin({
      dossier: this.mammaDossierService.getDossier(this.clientId),
      afspraak: this.mammaClientafspraakService.getGeplandeAfspraak(this.clientId),
      uitstel: this.mammaUitstelService.getActiefUitstel(this.clientId),
      acties: this.mammaClientafspraakService.getAfspraakActies(this.clientId),
    })
      .pipe(take(1))
      .subscribe(({ dossier, ...gegevens }) => this.mammaAfspraakGegevens.set(this.isMammaAfspraakZichtbaar(dossier, gegevens.afspraak) ? gegevens : null))
  }

  private isMammaAfspraakZichtbaar(dossier: MammaDossierDto | null, afspraak: MammaAfspraakDto | null) {
    const heeftDossierZonderSelectieblokkade = dossier !== null && dossier.deelnamemodus !== Deelnamemodus.SELECTIEBLOKKADE
    return heeftDossierZonderSelectieblokkade || afspraak !== null
  }

  protected getColonAfspraak() {
    this.colonIntakeafspraakService
      .getGeplandeAfspraak(this.clientId)
      .pipe(
        switchMap((afspraak) =>
          this.colonIntakeafspraakService.getAfspraakActies(this.clientId, afspraak?.id).pipe(
            map((acties) => ({
              type: Bevolkingsonderzoek.COLON,
              afspraak,
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
