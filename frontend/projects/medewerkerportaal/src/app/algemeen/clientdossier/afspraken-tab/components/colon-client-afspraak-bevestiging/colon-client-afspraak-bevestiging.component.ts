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
import { Component, inject, input, model } from '@angular/core'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { FormBuilder, ReactiveFormsModule } from '@angular/forms'
import { DsRadiobuttonComponent, DsRadiobuttonOption } from '@topicus-rgp-ds/web'
import { BriefType, briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ClientService } from '@algemeen/services/client/client.service'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'

@Component({
  selector: 'app-colon-client-afspraak-bevestiging',
  imports: [NotitieAanmakenVeldComponent, ReactiveFormsModule, DsRadiobuttonComponent, AutorisatieDirective],
  templateUrl: './colon-client-afspraak-bevestiging.component.html',
  styleUrl: './colon-client-afspraak-bevestiging.component.scss',
})
export class ColonClientAfspraakBevestigingComponent {
  afspraakslot = input<VrijSlotZonderKamerDto | null>()
  huidigeAfspraak = input<ClientAfspraakDto | null>()
  request = model<ColonAfspraakMakenRequestDto | null>(null)

  private readonly autorisatieService = inject(AutorisatieService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientService = inject(ClientService)

  protected form = this.formBuilder.group({
    briefType: this.formBuilder.control<BriefType | null | undefined>({ value: undefined, disabled: false }),
    notitie: '',
  })

  brieven: DsRadiobuttonOption<BriefType | null>[] = [
    {
      label: briefTypeLabels[BriefType.COLON_INTAKE_GEWIJZIGD],
      value: BriefType.COLON_INTAKE_GEWIJZIGD,
    },
    {
      label: briefTypeLabels[BriefType.COLON_UITNODIGING_INTAKE],
      value: BriefType.COLON_UITNODIGING_INTAKE,
    },
  ]
  briefConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF],
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    required: Required.ANY,
    actie: Actie.AANPASSEN,
  }

  constructor() {
    if (
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN],
        actie: Actie.AANPASSEN,
        required: Required.ALL,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
      })
    ) {
      this.brieven.push({
        label: 'Geen bevestigingsbrief versturen',
        value: null,
      })
    }

    this.form.valueChanges.pipe(takeUntilDestroyed()).subscribe((formValue) => {
      this.request.set({
        notitie: formValue.notitie ?? null,
        briefType: formValue.briefType,
        afspraakslot: this.afspraakslot()!,
        clientId: this.clientService.clientId(),
        afspraakId: this.huidigeAfspraak()!.id,
      })
    })
  }
}
