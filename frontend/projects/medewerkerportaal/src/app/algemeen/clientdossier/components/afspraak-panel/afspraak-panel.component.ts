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
import { faCalendar } from '@fortawesome/pro-solid-svg-icons'
import { DsBadgeComponent, DsButtonComponent, DsCardComponent, DsFooterActionsLeftDirective, DsFooterActionsRightDirective } from '@topicus-rgp-ds/web'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { DatumNaarWeekPipe } from '@shared/pipes/datum-naar-week/datum-naar-week.pipe'
import { NL_LONG_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ClientAfspraakEnActies } from '@shared/types/algemeen/client-afspraak-en-acties'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@shared/types/bevolkingsonderzoek'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'
import { StringsService } from '@shared/strings'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { ColonClientAfspraakAfzeggenDialogComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-afzeggen-dialog/colon-client-afspraak-afzeggen-dialog.component'
import { Dialog } from '@angular/cdk/dialog'
import { ClientService } from '@algemeen/services/client/client.service'
import { filter, take } from 'rxjs'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'

type AfspraakEnActies = ClientAfspraakEnActies<ColonIntakeafspraakDto> | ClientAfspraakEnActies<MammaAfspraakDto>

@Component({
  selector: 'app-afspraak-panel',
  imports: [
    DatumNaarWeekPipe,
    DatumTijdPipe,
    DsBadgeComponent,
    DsButtonComponent,
    DsCardComponent,
    EmptyStatePanelComponent,
    DsFooterActionsLeftDirective,
    DsFooterActionsRightDirective,
    LegeWaardePipe,
    AutorisatieDirective,
  ],
  templateUrl: './afspraak-panel.component.html',
  styleUrl: './afspraak-panel.component.scss',
})
export class AfspraakPanel {
  private readonly dialog = inject(Dialog)
  private readonly clientId = inject(ClientService).clientId()

  afspraakEnActies = input<AfspraakEnActies | null>(null)
  bvo = input.required<Bevolkingsonderzoek>()

  tijdstipWijzigen = output<ClientAfspraakDto>()
  locatieWijzigen = output<ClientAfspraakDto>()
  afspraakMaken = output<void>()
  afspraakAfgezegd = output<void>()

  protected readonly colonIntakeafspraak = computed(() => {
    const afspraakEnActies = this.afspraakEnActies()
    return this.isColonIntakeafspraak(afspraakEnActies) ? afspraakEnActies.afspraak : null
  })
  protected readonly afspraak = computed(() => this.afspraakEnActies()?.afspraak as ColonIntakeafspraakDto)
  protected readonly magNieuweAfspraakMaken = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.MAKEN) ?? false)
  protected readonly magTijdstipWijzigen = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.VERPLAATSEN) ?? false)
  protected readonly magLocatieWijzigen = computed(() => this.afspraakEnActies()?.acties?.includes(AfspraakActie.LOCATIE_WIJZIGEN) ?? false)
  protected readonly titel = computed(() => bevolkingsonderzoekLabels[this.bvo()])
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly faCalendar = faCalendar
  protected readonly NL_LONG_DATE_FORMAT = NL_LONG_DATE_FORMAT
  protected readonly TIME_FORMAT = TIME_FORMAT

  private readonly afspraakAanpassenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    required: Required.ANY,
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

  private isColonIntakeafspraak(afspraakEnActies: AfspraakEnActies | null): afspraakEnActies is ClientAfspraakEnActies<ColonIntakeafspraakDto> {
    return afspraakEnActies?.type === Bevolkingsonderzoek.COLON
  }
}
