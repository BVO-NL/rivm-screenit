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
import { format, parseISO } from 'date-fns'
import { filter, take } from 'rxjs'
import { faPaperPlane } from '@fortawesome/pro-solid-svg-icons'
import {
  DsButtonComponent,
  DsCardComponent,
  DsCardHeaderContentDirective,
  DsEmptyState,
  DsEmptyStateComponent,
  DsEmptyStateStatus,
  DsFooterActionsRightDirective,
  DsIconComponent,
} from '@topicus-rgp-ds/web'
import { AfspraakGegevensComponent } from '@algemeen/clientdossier/components/afspraak-gegevens/afspraak-gegevens.component'
import { NL_DATE_FORMAT, NL_DAY_DATE_FORMAT } from '@shared/constants'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@shared/types/bevolkingsonderzoek'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'
import { MammaUitstelDto } from '@shared/types/mamma/dto/mamma-uitstel.dto'
import { AfspraakActie } from '@shared/types/algemeen/enum/afspraak-actie'
import { StringsService } from '@shared/strings'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Dialog } from '@angular/cdk/dialog'
import {
  BevestigingKeuze,
  MammaAfspraakbevestigingDialogComponent,
  MammaAfspraakbevestigingDialogData,
} from '@algemeen/clientdossier/afspraken-tab/components/mamma-afspraakbevestiging-dialog/mamma-afspraakbevestiging-dialog.component'

const AFSPRAAK_MAKEN_CONSTRAINT: SecurityConstraint = {
  recht: [Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_MAKEN],
  actie: Actie.TOEVOEGEN,
  bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
  required: Required.ANY,
}

const AFSPRAAK_WIJZIGEN_INZIEN_CONSTRAINT: SecurityConstraint = {
  recht: [Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN],
  actie: Actie.INZIEN,
  bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
  required: Required.ANY,
}

const AFSPRAAK_WIJZIGEN_CONSTRAINT: SecurityConstraint = {
  recht: [Recht.MEDEWERKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN],
  actie: Actie.AANPASSEN,
  bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
  required: Required.ANY,
}

@Component({
  selector: 'app-mamma-afspraak-panel',
  imports: [AfspraakGegevensComponent, DsButtonComponent, DsCardComponent, DsCardHeaderContentDirective, DsEmptyStateComponent, DsFooterActionsRightDirective, DsIconComponent],
  templateUrl: './mamma-afspraak-panel.component.html',
  styleUrl: './mamma-afspraak-panel.component.scss',
})
export class MammaAfspraakPanelComponent {
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly dialog = inject(Dialog)

  afspraak = input<MammaAfspraakDto | null>(null)
  uitstel = input<MammaUitstelDto | null>(null)
  acties = input<AfspraakActie[]>([])
  bevestigingsbriefAangevraagd = output<void>()

  protected readonly strings = inject(StringsService).getStrings()
  protected readonly afspraakStrings = this.strings.algemeen.afspraken
  protected readonly titel = bevolkingsonderzoekLabels[Bevolkingsonderzoek.MAMMA]
  protected readonly faPaperPlane = faPaperPlane

  private readonly magAfspraakMaken = this.autorisatieService.isToegestaan(AFSPRAAK_MAKEN_CONSTRAINT)
  private readonly magAfspraakWijzigen = this.autorisatieService.isToegestaan(AFSPRAAK_WIJZIGEN_CONSTRAINT)
  private readonly magAfspraakbevestigingSturen = this.magAfspraakMaken && this.autorisatieService.isToegestaan(AFSPRAAK_WIJZIGEN_INZIEN_CONSTRAINT)

  protected readonly toonAfspraakbevestigingSturen = computed(
    () => this.magAfspraakbevestigingSturen && this.afspraak() !== null && this.acties().includes(AfspraakActie.AFSPRAAKBEVESTIGING_STUREN),
  )
  protected readonly toonVerzetten = computed(() => this.magAfspraakWijzigen && this.afspraak() !== null && this.acties().includes(AfspraakActie.VERPLAATSEN))
  protected readonly toonUitstellen = computed(() => this.magAfspraakWijzigen && this.acties().includes(AfspraakActie.UITSTELLEN))
  protected readonly toonAfspraakMaken = computed(() => this.magAfspraakMaken && this.afspraak() === null && this.acties().includes(AfspraakActie.MAKEN))

  protected readonly tijdelijkeLocatieTekst = computed(() => {
    const afspraak = this.afspraak()
    if (!afspraak?.tijdelijkeLocatieVanaf || !afspraak.tijdelijkeLocatieTotEnMet) {
      return null
    }
    return this.afspraakStrings.mamma.tijdelijkeLocatie(
      format(parseISO(afspraak.tijdelijkeLocatieVanaf), NL_DATE_FORMAT),
      format(parseISO(afspraak.tijdelijkeLocatieTotEnMet), NL_DATE_FORMAT),
    )
  })

  protected readonly geenAfspraakState: DsEmptyState = {
    title: this.afspraakStrings.geenAfspraak.titel,
    subtitle: this.afspraakStrings.geenAfspraak.subtitel,
    status: DsEmptyStateStatus.Info,
  }

  protected readonly uitstelIngediendState = computed<DsEmptyState | null>(() => {
    const uitstel = this.uitstel()
    if (!uitstel) {
      return null
    }
    const uitstelIngediendStrings = this.afspraakStrings.mamma.uitstelIngediend
    const streefdatum = format(parseISO(uitstel.streefDatum), NL_DAY_DATE_FORMAT).replaceAll('-', '‑')
    return {
      title: uitstelIngediendStrings.titel,
      subtitle: uitstelIngediendStrings.subtitel(streefdatum, uitstel.standplaatsNaam, uitstel.plaats),
      status: DsEmptyStateStatus.Info,
    }
  })

  protected openAfspraakbevestigingDialog() {
    this.dialog
      .open<BevestigingKeuze, MammaAfspraakbevestigingDialogData>(MammaAfspraakbevestigingDialogComponent, { data: { afspraak: this.afspraak()! } })
      .closed.pipe(
        take(1),
        filter((keuze) => keuze === 'BRIEF'),
      )
      .subscribe(() => this.bevestigingsbriefAangevraagd.emit())
  }
}
