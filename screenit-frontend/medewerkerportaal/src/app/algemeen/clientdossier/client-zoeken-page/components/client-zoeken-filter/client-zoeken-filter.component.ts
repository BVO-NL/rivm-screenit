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
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { DsButtonComponent, DsDatepickerComponent, DsIconComponent, DsInputComponent, DsToggleComponent, DsValidators } from '@topicus-rgp-ds/web'
import { afterNextRender, Component, ElementRef, inject, output, signal, viewChild } from '@angular/core'
import { FormBuilder, FormControl, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms'
import { faSearch } from '@fortawesome/pro-light-svg-icons'
import { ClientZoekenFilterDto } from '@shared/types/algemeen/dto/client-zoeken-filter.dto'
import { DateAdapter } from '@angular/material/core'
import { ScreenitDateFnsAdapter } from '@shared/adapters/screenit-date-fns-adapter'
import { NotificationService } from '@shared/services/notification/notification.service'
import { briefkenmerkValidator, huisnummerValidator, telefoonnummerValidator, trimmedValidator } from '@shared/validators/common-validators'
import { createMaximumDatumValidator } from '@shared/validators/datum/datum.validator'
import { AlleenCijfersDirective } from '@shared/directives/alleen-cijfers/alleen-cijfers.directive'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { Required } from '@shared/types/autorisatie/required'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'

@Component({
  selector: 'app-client-zoeken-filter',
  imports: [
    AlleenCijfersDirective,
    AutorisatieDirective,
    DsButtonComponent,
    DsDatepickerComponent,
    DsIconComponent,
    DsInputComponent,
    FormsModule,
    ReactiveFormsModule,
    DsToggleComponent,
  ],
  providers: [{ provide: DateAdapter, useClass: ScreenitDateFnsAdapter }],
  templateUrl: './client-zoeken-filter.component.html',
  styleUrl: './client-zoeken-filter.component.scss',
})
export class ClientZoekenFilterComponent {
  protected readonly searchIcon = faSearch
  private readonly formBuilder = inject(FormBuilder)
  private readonly notificationService = inject(NotificationService)
  private readonly geboortedatumPicker = viewChild('geboortedatum', { read: ElementRef })
  private readonly minimumGeboortedatum = new FormControl(new Date())
  zoeken = output<ClientZoekenFilterDto>()
  geavanceerdZoekenActief = signal(false)
  protected readonly geavanceerdZoekenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_ZOEKEN_UITGEBREID],
    actie: Actie.INZIEN,
    required: Required.ANY,
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
  }

  protected geavanceerdZoekenControl = new FormControl<boolean>(false, { nonNullable: true })

  zoekenForm = this.formBuilder.group({
    geboortedatum: this.formBuilder.control<Date | null>(null, [Validators.required, createMaximumDatumValidator(this.minimumGeboortedatum, 0)]),
    bsn: this.formBuilder.control<string | null>(null, trimmedValidator(DsValidators.bsn)),
    postcode: this.formBuilder.control<string | null>(null, trimmedValidator(DsValidators.postcode)),
    huisnummer: this.formBuilder.control<string | null>(null, huisnummerValidator),
    briefkenmerk: this.formBuilder.control<string | null>(null, briefkenmerkValidator),
    bkUitnodigingsnummer: this.formBuilder.control<number | null>(null),
    bmhkMonsterId: this.formBuilder.control<string | null>(null),
    bmhkUitnodigingsId: this.formBuilder.control<number | null>(null),
    dkBarcode: this.formBuilder.control<string | null>(null),
    dkUitnodigingsId: this.formBuilder.control<number | null>(null),
    anummer: this.formBuilder.control<string | null>(null),
    mobielnummer: this.formBuilder.control<string | null>(null, telefoonnummerValidator),
    emailadres: this.formBuilder.control<string | null>(null, [Validators.email]),
  })

  constructor() {
    afterNextRender(() => {
      this.geboortedatumPicker()?.nativeElement?.querySelector('input')?.focus()
    })

    this.zoekenForm
      .get('briefkenmerk')!
      .valueChanges.pipe(takeUntilDestroyed())
      .subscribe((waarde) => {
        if (waarde) {
          const zonderSpaties = waarde.replace(/\s/g, '').toUpperCase()
          if (zonderSpaties !== waarde) {
            this.zoekenForm.controls.briefkenmerk.setValue(zonderSpaties, { emitEvent: false })
          }
        }
      })

    this.geavanceerdZoekenControl.valueChanges.pipe(takeUntilDestroyed()).subscribe((actief) => {
      this.geavanceerdZoekenActief.set(actief ?? false)
      if (actief) {
        this.geboortedatumControl.setValidators(createMaximumDatumValidator(this.minimumGeboortedatum, 0))
        this.geboortedatumControl.updateValueAndValidity()
      } else {
        this.geboortedatumControl.setValidators([Validators.required, createMaximumDatumValidator(this.minimumGeboortedatum, 0)])
        this.geboortedatumControl.updateValueAndValidity()
        this.zoekenForm.patchValue(
          {
            bkUitnodigingsnummer: null,
            bmhkMonsterId: null,
            bmhkUitnodigingsId: null,
            dkBarcode: null,
            dkUitnodigingsId: null,
            anummer: null,
            mobielnummer: null,
            emailadres: null,
          },
          { emitEvent: false },
        )
      }
    })
  }

  get geboortedatumControl() {
    return this.zoekenForm.get('geboortedatum') as FormControl
  }

  zoekClienten() {
    this.zoekenForm.markAllAsTouched()

    const FOUTMELDING_POSTCODE_HUISNUMMER = 'Vul zowel postcode als huisnummer in'
    const FOUTMELDING_ONGELDIGE_INPUT =
      this.geavanceerdZoekenActief() && this.heeftGeenBasisVeldenGevuld()
        ? 'Vul minstens 1 veld in om een cliënt te zoeken'
        : 'Vul geboortedatum en minstens 1 extra veld in om een cliënt te zoeken'

    if (!this.heeftGeldigePostcodeHuisnummerCombinatie()) {
      this.notificationService.error(FOUTMELDING_POSTCODE_HUISNUMMER)
      return
    }

    this.notificationService.hide(FOUTMELDING_POSTCODE_HUISNUMMER)

    if (this.zoekenForm.valid && this.heeftGeldigeInput()) {
      this.notificationService.hide(FOUTMELDING_ONGELDIGE_INPUT)
      const formWaarden = Object.fromEntries(
        Object.entries(this.zoekenForm.value).map(([key, value]) => [key, typeof value === 'string' ? value.trim() : value]),
      )
      this.zoeken.emit(formWaarden as typeof this.zoekenForm.value)
    } else if (this.zoekenForm.valid && !this.heeftGeldigeInput()) {
      this.notificationService.error(FOUTMELDING_ONGELDIGE_INPUT)
    }
  }

  protected heeftGeldigePostcodeHuisnummerCombinatie(): boolean {
    const { postcode, huisnummer } = this.zoekenForm.value
    const beideIngevuld = Boolean(postcode) && Boolean(huisnummer)
    const beideLeeg = !Boolean(postcode) && !Boolean(huisnummer)
    return beideIngevuld || beideLeeg
  }

  protected heeftGeldigeInput(): boolean {
    if (this.geavanceerdZoekenActief() && this.heeftGeenBasisVeldenGevuld()) {
      return this.heeftGeldigeGeavanceerdeCombinatie()
    }
    const { geboortedatum } = this.zoekenForm.value
    return !!geboortedatum && this.heeftGeldigeCombinatie()
  }

  protected heeftGeldigeCombinatie(): boolean {
    const { bsn, postcode, huisnummer, briefkenmerk } = this.zoekenForm.value
    const heeftAdres = Boolean(postcode) && Boolean(huisnummer)
    const heeftBsn = Boolean(bsn)
    const heeftBriefkenmerk = Boolean(briefkenmerk)
    return heeftAdres || heeftBsn || heeftBriefkenmerk
  }

  private heeftGeenBasisVeldenGevuld(): boolean {
    const { bsn, postcode, huisnummer, briefkenmerk, geboortedatum } = this.zoekenForm.value
    return !geboortedatum && !bsn && !postcode && !huisnummer && !briefkenmerk
  }

  private heeftGeldigeGeavanceerdeCombinatie(): boolean {
    const { bkUitnodigingsnummer, bmhkMonsterId, bmhkUitnodigingsId, dkBarcode, dkUitnodigingsId, anummer, mobielnummer, emailadres } = this.zoekenForm.value
    return !!bkUitnodigingsnummer || !!bmhkMonsterId || !!bmhkUitnodigingsId || !!dkBarcode || !!dkUitnodigingsId || !!anummer || !!mobielnummer || !!emailadres
  }
}
