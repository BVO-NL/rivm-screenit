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
import { Component, computed, effect, inject, input, model, untracked } from '@angular/core'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { DsButtongroupComponent, DsRadiobuttonComponent, DsRadiobuttonOption } from '@topicus-rgp-ds/web'
import { BriefType, briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { toSignal } from '@angular/core/rxjs-interop'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { ColonIntakeafspraakAanmakenType } from '@shared/types/colon/enum/colon-intakeafspraak-aanmaken-type'
import { StringsService } from '@shared/strings'
import { ColonClientAfspraakDialogMode } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-dialog/colon-client-afspraak-dialog.component'
import { ClientService } from '@algemeen/services/client/client.service'

@Component({
  selector: 'app-colon-client-afspraak-bevestiging',
  imports: [NotitieAanmakenVeldComponent, ReactiveFormsModule, DsRadiobuttonComponent, DsButtongroupComponent],
  templateUrl: './colon-client-afspraak-bevestiging.component.html',
})
export class ColonClientAfspraakBevestigingComponent {
  afspraakslot = input<VrijSlotZonderKamerDto | null>()
  afspraakType = input<ColonIntakeafspraakAanmakenType | null>()
  huidigeAfspraak = input<ClientAfspraakDto | null>()
  request = model<ColonAfspraakMakenRequestDto | null>(null)
  afspraakDialogMode = input<ColonClientAfspraakDialogMode | null>(null)

  private readonly autorisatieService = inject(AutorisatieService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientService = inject(ClientService)
  protected readonly strings = inject(StringsService).getStrings()

  readonly isSpoed = computed(() => this.afspraakType() === ColonIntakeafspraakAanmakenType.SPOED)

  readonly isBuitenGeplandeSlots = computed(() => this.afspraakType() === ColonIntakeafspraakAanmakenType.BUITEN)

  private readonly geenBevestigingsbriefStandaard = computed(() => this.isSpoed() || this.isBuitenGeplandeSlots())

  readonly brieven = computed<DsRadiobuttonOption<BriefType | null>[]>(() => {
    const brieven: DsRadiobuttonOption<BriefType | null>[] = [
      {
        label: briefTypeLabels[BriefType.COLON_INTAKE_GEWIJZIGD],
        value: BriefType.COLON_INTAKE_GEWIJZIGD,
      },
    ]

    if (this.magRecht(Recht.MEDEWERKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF)) {
      brieven.push({
        label: briefTypeLabels[BriefType.COLON_UITNODIGING_INTAKE],
        value: BriefType.COLON_UITNODIGING_INTAKE,
      })
    }

    if (this.geenBevestigingsbriefStandaard() || this.magRecht(Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN)) {
      brieven.push({
        label: this.strings.algemeen.afspraken.verplaatsenDialog.bevestigingStep.geenBevestigingsbrief,
        value: null,
      })
    }

    return brieven
  })

  protected readonly defaultBriefType = computed<BriefType | null>(() => {
    return this.geenBevestigingsbriefStandaard() ? null : this.brieven()[0].value
  })

  form = this.formBuilder.group({
    briefType: this.formBuilder.control<BriefType | null>(this.defaultBriefType()),
    verwezenMedischeRedenen: this.formBuilder.control<boolean | null>(null),
    notitie: '',
  })

  private readonly formWaarde = toSignal(this.form.valueChanges, { initialValue: this.form.value })

  protected readonly jaNeeOpties: DsRadiobuttonOption<boolean>[] = [
    { label: this.strings.algemeen.labels.ja, value: true },
    { label: this.strings.algemeen.labels.nee, value: false },
  ]

  protected readonly toonMedischeVerwijzing = computed(() => {
    const huidigeAfspraak = this.huidigeAfspraak()
    const afspraakslot = this.afspraakslot()
    return huidigeAfspraak != null && afspraakslot != null && huidigeAfspraak.verlopenZonderConclusie && huidigeAfspraak.locatie.id !== afspraakslot.intakelocatie.id
  })

  toonValidatiefouten() {
    this.form.markAllAsTouched()
  }

  herstelStandaardwaarden() {
    this.form.reset({ briefType: this.defaultBriefType(), verwezenMedischeRedenen: null, notitie: '' })
  }

  constructor() {
    effect(() => {
      const verwezenMedischeRedenenControl = this.form.get('verwezenMedischeRedenen') as FormControl<boolean | null>
      verwezenMedischeRedenenControl.setValidators(this.toonMedischeVerwijzing() ? Validators.required : [])
      verwezenMedischeRedenenControl.updateValueAndValidity({ emitEvent: false })
    })

    effect(() => {
      this.form.controls.briefType.setValue(this.defaultBriefType())
    })

    effect(() => {
      const formWaarde = this.formWaarde()
      const afspraakslot = this.afspraakslot()!
      const huidigeAfspraak = this.huidigeAfspraak()
      const toonMedischeVerwijzing = this.toonMedischeVerwijzing()

      const request = untracked(() => this.request())
      if (!request) {
        return
      }

      this.request.set({
        ...request,
        notitie: formWaarde.notitie || null,
        briefType: formWaarde.briefType ?? null,
        verwezenMedischeRedenenDoorInfolijn: toonMedischeVerwijzing ? (formWaarde.verwezenMedischeRedenen ?? null) : null,
        afspraakslot,
        clientId: this.clientService.clientId(),
        afspraakId: huidigeAfspraak?.id,
      })
    })
  }

  private magRecht(recht: Recht): boolean {
    return this.autorisatieService.isToegestaan({
      recht: [recht],
      actie: Actie.AANPASSEN,
      required: Required.ALL,
      bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    })
  }
}
