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
import { Component, computed, inject, input, output } from '@angular/core'
import {
  DsButtonComponent,
  DsCardComponent,
  DsEmptyState,
  DsEmptyStateComponent,
  DsEmptyStateStatus,
  DsFooterActionsLeftDirective,
  DsFooterActionsRightDirective,
} from '@topicus-rgp-ds/web'
import { ClientAfspraakEnActies } from '@shared/types/algemeen/client-afspraak-en-acties'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@shared/types/bevolkingsonderzoek'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'
import { StringsService } from '@shared/strings'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { ColonClientAfspraakAfzeggenDialogComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-afzeggen-dialog/colon-client-afspraak-afzeggen-dialog.component'
import { Dialog } from '@angular/cdk/dialog'
import { ClientService } from '@algemeen/services/client/client.service'
import { filter, take } from 'rxjs'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { AfspraakGegevensComponent } from '@algemeen/clientdossier/components/afspraak-gegevens/afspraak-gegevens.component'

@Component({
  selector: 'app-afspraak-panel',
  imports: [
    AfspraakGegevensComponent,
    DsButtonComponent,
    DsCardComponent,
    DsEmptyStateComponent,
    DsFooterActionsLeftDirective,
    DsFooterActionsRightDirective,
    AutorisatieDirective,
  ],
  templateUrl: './afspraak-panel.component.html',
  styleUrl: './afspraak-panel.component.scss',
})
export class AfspraakPanel {
  private readonly dialog = inject(Dialog)
  private readonly clientId = inject(ClientService).clientId()

  afspraakEnActies = input<ClientAfspraakEnActies<ColonIntakeafspraakDto> | null>(null)
  bvo = input.required<Bevolkingsonderzoek>()

  tijdstipWijzigen = output<ClientAfspraakDto>()
  locatieWijzigen = output<ClientAfspraakDto>()
  afspraakMaken = output<void>()
  afspraakAfgezegd = output<void>()

  protected readonly afspraak = computed(() => this.afspraakEnActies()?.afspraak ?? null)
  protected readonly magNieuweAfspraakMaken = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.MAKEN) ?? false)
  protected readonly magTijdstipWijzigen = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.VERPLAATSEN) ?? false)
  protected readonly magLocatieWijzigen = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.LOCATIE_WIJZIGEN) ?? false)
  protected readonly titel = computed(() => bevolkingsonderzoekLabels[this.bvo()])
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly geenAfspraakState: DsEmptyState = {
    title: this.strings.algemeen.afspraken.geenAfspraak.titel,
    subtitle: this.strings.algemeen.afspraken.geenAfspraak.subtitel,
    status: DsEmptyStateStatus.Info,
  }

  private readonly afspraakAfzeggenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    required: Required.ANY,
  }

  protected openAfspraakAfzeggenDialog(afspraak: ClientAfspraakDto) {
    this.dialog
      .open(ColonClientAfspraakAfzeggenDialogComponent, { data: { clientId: this.clientId, afspraak } })
      .closed.pipe(
        take(1),
        filter((afgezegd) => afgezegd === true),
      )
      .subscribe(() => this.afspraakAfgezegd.emit())
  }
}
