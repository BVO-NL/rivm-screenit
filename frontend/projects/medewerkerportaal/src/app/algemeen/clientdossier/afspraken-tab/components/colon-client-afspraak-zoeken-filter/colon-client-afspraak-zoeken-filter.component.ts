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
import { Component, computed, inject, input, output, signal } from '@angular/core'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { ColonAfspraakZoekopdracht } from '@shared/types/colon/colon-afspraak-zoekopdracht'
import {
  DropdownValueModel,
  DsButtonComponent,
  DsDatepickerComponent,
  DsDropdownComponent,
  DsIconComponent,
  DsInputComponent,
  DsOptionTemplateDirective,
} from '@topicus-rgp-ds/web'
import { ColonIntakeafspraakAanmakenType, colonIntakeafspraakAanmakenTypeLabels } from '@shared/types/colon/enum/colon-intakeafspraak-aanmaken-type'
import { faSearch } from '@fortawesome/pro-light-svg-icons'
import { ClientService } from '@algemeen/services/client/client.service'
import { ParameterService } from '@algemeen/services/parameter/parameter.service'
import { ParameterKey } from '@shared/types/algemeen/enum/parameter-key'
import { addBusinessDays, startOfDay } from 'date-fns'
import { RoosterService } from '@colon/colon-rooster-page/services/rooster.service'
import { take } from 'rxjs'
import { ColonZoekfilterInstellingen } from '@shared/types/colon/colon-zoekfilter-instellingen'
import { StringsService } from '@shared/strings'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { DatumTijdPickerComponent } from '@shared/components/datum-tijd-picker/datum-tijd-picker.component'
import { ColonClientAfspraakDialogMode } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-dialog/colon-client-afspraak-dialog.component'
import { datumTijdInVerledenValidator } from '@shared/validators/datum/datum.validator'
import { DateService } from '@shared/services/date/date.service'

@Component({
  selector: 'app-colon-client-afspraak-zoeken-filter',
  imports: [
    ReactiveFormsModule,
    DsDropdownComponent,
    DsDatepickerComponent,
    DatumTijdPickerComponent,
    DsButtonComponent,
    DsIconComponent,
    DsOptionTemplateDirective,
    DsInputComponent,
  ],
  templateUrl: './colon-client-afspraak-zoeken-filter.component.html',
  styleUrl: './colon-client-afspraak-zoeken-filter.component.scss',
})
export class ColonClientAfspraakZoekenFilterComponent {
  private static readonly AFSTANDEN_IN_KM = [5, 10, 15, 20, 25, 30, 35, 40, 45]

  private static readonly SELECTEERBARE_AFSPRAAKTYPES = [ColonIntakeafspraakAanmakenType.STANDAARD, ColonIntakeafspraakAanmakenType.SPOED, ColonIntakeafspraakAanmakenType.BUITEN]

  private readonly formBuilder = inject(FormBuilder)
  private readonly clientService = inject(ClientService)
  private readonly parameterService = inject(ParameterService)
  private readonly roosterService = inject(RoosterService)
  private readonly autorisatieService = inject(AutorisatieService)
  private instellingen: ColonZoekfilterInstellingen | null = null

  clientAfspraakDialogMode = input<ColonClientAfspraakDialogMode>(null)
  huidigeIntakelocatieId = input<number | null>(null)
  toonGeavanceerd = computed(() => this.clientAfspraakDialogMode() === 'locatie_wijzigen' || this.clientAfspraakDialogMode() === 'aanmaken')

  zoeken = output<ColonAfspraakZoekopdracht>()
  afspraakTypeChange = output<ColonIntakeafspraakAanmakenType>()
  datumTijdBuitenRoosterChange = output<string | null>()

  protected readonly afspraakTypeItems: DropdownValueModel<ColonIntakeafspraakAanmakenType>[] = ColonClientAfspraakZoekenFilterComponent.SELECTEERBARE_AFSPRAAKTYPES.map(
    (type) => ({
      label: colonIntakeafspraakAanmakenTypeLabels[type],
      value: type,
    }),
  )
  protected readonly afstandItems: DropdownValueModel<number>[] = ColonClientAfspraakZoekenFilterComponent.AFSTANDEN_IN_KM.map((afstand) => ({
    label: String(afstand),
    value: afstand,
  }))
  protected readonly searchIcon = faSearch
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly magAfwijkendTypePlannen = this.autorisatieService.isToegestaan({
    recht: [Recht.MEDEWERKER_CLIENT_SR_INTAKE_VERPLAATS_BINNEN_INTAKE_NIET_WIJZIGBAAR_PERIODE],
    actie: Actie.AANPASSEN,
    required: Required.ALL,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
  })

  get typeCtrl(): FormControl<ColonIntakeafspraakAanmakenType> {
    return this.zoekenForm.get('type') as FormControl<ColonIntakeafspraakAanmakenType>
  }

  get vanafCtrl(): FormControl<Date | null> {
    return this.zoekenForm.get('vanaf') as FormControl<Date | null>
  }

  get totEnMetCtrl(): FormControl<Date | null> {
    return this.zoekenForm.get('totEnMet') as FormControl<Date | null>
  }

  get datumTijdBuitenRoosterCtrl(): FormControl<string | null> {
    return this.zoekenForm.get('datumTijdBuitenRooster') as FormControl<string | null>
  }

  protected readonly minimumDatum = signal<string>('')

  zoekenForm = this.formBuilder.group({
    clientId: this.formBuilder.control(this.clientService.clientId(), { nonNullable: true }),
    type: this.formBuilder.control(ColonIntakeafspraakAanmakenType.STANDAARD, { nonNullable: true, validators: Validators.required }),
    vanaf: this.formBuilder.control<Date | null>(null, Validators.required),
    totEnMet: this.formBuilder.control<Date | null>(null, Validators.required),
    naam: this.formBuilder.control<string | null>(null),
    plaats: this.formBuilder.control<string | null>(null),
    afstand: this.formBuilder.control<number | null>(null),
    datumTijdBuitenRooster: this.formBuilder.control<string | null>(null, [Validators.required, datumTijdInVerledenValidator]),
  })

  private readonly gekozenType = toSignal(this.typeCtrl.valueChanges, { initialValue: this.typeCtrl.value })
  protected readonly isBuitenGeplandeSlots = computed(() => this.gekozenType() === ColonIntakeafspraakAanmakenType.BUITEN)

  protected readonly isTijdstipWijzigenBuitenGeplandeSlots = computed(() => this.isBuitenGeplandeSlots() && this.clientAfspraakDialogMode() === 'tijdstip_wijzigen')
  protected readonly toonDatumTijdBuitenRooster = computed(() => this.isBuitenGeplandeSlots() && !this.isTijdstipWijzigenBuitenGeplandeSlots())

  constructor() {
    this.zetFilters(this.typeCtrl.value)

    this.roosterService
      .getInstellingen()
      .pipe(take(1))
      .subscribe((instellingen) => {
        this.instellingen = {
          geprognosticeerdeIntakeDatum: instellingen.geprognosticeerdeIntakeDatum,
          nietWijzigbarePeriode: this.getNietWijzigbarePeriode(),
        }
        this.zetFilters(this.typeCtrl.value)
      })

    this.typeCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((type) => {
      this.zetFilters(type)
      this.afspraakTypeChange.emit(type)
    })

    this.datumTijdBuitenRoosterCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((datum) => this.datumTijdBuitenRoosterChange.emit(datum))
  }

  private zetFilters(type: ColonIntakeafspraakAanmakenType) {
    const buitenGeplandeSlots = type === ColonIntakeafspraakAanmakenType.BUITEN
    if (buitenGeplandeSlots && !this.isTijdstipWijzigenBuitenGeplandeSlots()) {
      this.datumTijdBuitenRoosterCtrl.enable()
    } else {
      this.datumTijdBuitenRoosterCtrl.disable()
    }

    if (!this.instellingen) {
      return
    }

    const nietWijzigbarePeriode = this.instellingen.nietWijzigbarePeriode
    this.minimumDatum.set(type === ColonIntakeafspraakAanmakenType.STANDAARD ? addBusinessDays(DateService.nu(), nietWijzigbarePeriode).toISOString() : '')

    if (type === ColonIntakeafspraakAanmakenType.SPOED) {
      const vanaf = startOfDay(DateService.nu())

      this.vanafCtrl.setValue(vanaf)
      this.totEnMetCtrl.setValue(addBusinessDays(vanaf, nietWijzigbarePeriode - 1))
      this.vanafCtrl.disable()
      this.totEnMetCtrl.disable()
    } else if (buitenGeplandeSlots) {
      this.vanafCtrl.reset(null)
      this.totEnMetCtrl.reset(null)
      this.vanafCtrl.disable()
      this.totEnMetCtrl.disable()
    } else {
      this.vanafCtrl.enable()
      this.totEnMetCtrl.enable()
      this.vanafCtrl.setValue(new Date(this.minimumDatum()))
      this.totEnMetCtrl.setValue(this.instellingen.geprognosticeerdeIntakeDatum)
    }
  }

  private getNietWijzigbarePeriode(): number {
    return Number(this.parameterService.getParameterWaarde<number>(ParameterKey.INTAKE_NIET_WIJZIGBAAR, 3))
  }

  verstuurQuery() {
    if (this.zoekenForm.invalid) {
      this.zoekenForm.markAllAsTouched()
      return
    }

    const { clientId, vanaf, totEnMet, naam, plaats, afstand, datumTijdBuitenRooster } = this.zoekenForm.getRawValue()

    this.zoeken.emit({
      filter: {
        clientId,
        vanaf: vanaf!,
        totEnMet: totEnMet!,
        naam,
        plaats,
        afstand,
        intakelocatieId: this.clientAfspraakDialogMode() === 'locatie_wijzigen' ? null : this.huidigeIntakelocatieId(),
        alleenIntakelocaties: this.isBuitenGeplandeSlots(),
      },
      datumTijdBuitenRooster: this.isBuitenGeplandeSlots() ? datumTijdBuitenRooster : null,
    })
  }
}
