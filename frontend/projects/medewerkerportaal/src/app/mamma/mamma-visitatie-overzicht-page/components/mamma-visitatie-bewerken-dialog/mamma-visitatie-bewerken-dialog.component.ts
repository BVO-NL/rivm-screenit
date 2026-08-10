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
import { Component, effect, EffectRef, inject, OnDestroy } from '@angular/core'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { MammaVisitatieStatus, mammaVisitatieStatusLijst } from '@shared/types/mamma/mamma-visitatie-status'
import { OrganisatieService } from '@/algemeen/services/organisatie/organisatie.service'
import { toSignal } from '@angular/core/rxjs-interop'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { aantalBestandenValidator, extensieValidator } from '@shared/validators/file/file.validator'
import { Recht } from '@shared/types/autorisatie/recht'
import { EnumOptie } from '@shared/types/enum-optie'
import { isEmptyObject } from '@shared/utils/object-utils'
import { MammaVisitatieService } from '@/mamma/mamma-visitatie-overzicht-page/services/mamma-visitatie.service'
import { take } from 'rxjs'
import { MammaVisitatieOnderdeel } from '@shared/types/mamma/mamma-visitatie-onderdeel'
import { OrganisatieDto } from '@shared/types/algemeen/dto/organisatie.dto'
import { MammaVisitatieRequestDto } from '@shared/types/mamma/dto/visitatie/mamma-visitatie-request.dto'
import { uniekValidator } from '@shared/validators/uniek/uniek.validator'
import { MammaVisitatieResponseDto } from '@shared/types/mamma/dto/visitatie/mamma-visitatie-response.dto'
import { NotificationService } from '@shared/services/notification/notification.service'
import { MammaFotorichting } from '@shared/types/mamma/mamma-fotorichting'
import { MammaVisitatieDto } from '@shared/types/mamma/dto/visitatie/mamma-visitatie.dto'
import { DsButtonComponent, DsDropdownComponent, DsInputComponent, DsRadiobuttonComponent, DsRadiobuttonOption } from '@topicus-rgp-ds/web'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'

@Component({
  selector: 'app-mamma-visitatie-bewerken-dialog',
  imports: [BaseDialogComponent, ReactiveFormsModule, DsInputComponent, DsDropdownComponent, DsRadiobuttonComponent, DsButtonComponent, SingleFileSelectorComponent],
  templateUrl: './mamma-visitatie-bewerken-dialog.component.html',
  styleUrl: './mamma-visitatie-bewerken-dialog.component.scss',
})
export class MammaVisitatieBewerkenDialogComponent implements OnDestroy {
  private readonly dialogRef = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly organisatieService = inject(OrganisatieService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly visitatieService = inject(MammaVisitatieService)
  private notificationService: NotificationService = inject(NotificationService)

  private cleanupEffect: EffectRef
  private readonly dialogData: { visitatieType: string; visitatie: MammaVisitatieDto } = inject(DIALOG_DATA)
  protected readonly visitatie = this.dialogData.visitatie
  protected readonly visitatieType = this.dialogData.visitatieType
  protected beoordelingseenheden = toSignal(this.organisatieService.getBeoordelingseenheden(), { initialValue: [] })
  protected mammaVisitatieStatusLijst: EnumOptie<MammaVisitatieStatus>[] = [...mammaVisitatieStatusLijst]
  protected readonly fotorichtingen = Object.values(MammaFotorichting)

  get beoordelingseenheidCtrl(): FormControl {
    return this.editForm.get('beoordelingseenheid') as FormControl
  }

  get fotorichtingCtrl(): FormControl {
    return this.editForm.get('fotorichting') as FormControl
  }

  get isNieuw(): boolean {
    return !this.visitatie.gestartOp
  }

  get visitatieTypeBe(): boolean {
    return this.visitatieType === 'be'
  }

  editForm = this.formBuilder.group({
    omschrijving: ['', [Validators.required], [uniekValidator(this.visitatieService.getVisitatieByOmschrijving.bind(this.visitatieService))]],
    status: [MammaVisitatieStatus.INGEPLAND, Validators.required],
    beoordelingseenheid: this.formBuilder.control<OrganisatieDto | null>(null, Validators.required),
    insteltechniek: this.getNewFileControl(),
    fotorichting: this.formBuilder.control<EnumOptie<MammaFotorichting> | null>(null),
    intervalcarcinomen: this.getNewFileControl(),
    screenDetected: this.getNewFileControl(),
    verwijzingen: this.getNewFileControl(),
    protheses: this.getNewFileControl(),
    rapportage: this.getNewFileControl(),
    vragenlijst: this.getNewFileControl(),
  })

  constructor() {
    this.cleanupEffect = effect(() => {
      if (this.visitatie && !isEmptyObject(this.visitatie)) {
        this.editForm.patchValue({
          omschrijving: this.visitatie.omschrijving,
          status: this.visitatie.status,
          beoordelingseenheid: this.getBeoordelingseenheid(this.visitatie.beoordelingseenheid?.id),
        })

        if (this.isNieuw && [MammaVisitatieStatus.INGEPLAND, MammaVisitatieStatus.VRIJGEGEVEN].includes(this.visitatie.status)) {
          this.mammaVisitatieStatusLijst = [...mammaVisitatieStatusLijst].filter((status) => status.waarde !== MammaVisitatieStatus.UITGEVOERD)
        }
      }
    })

    this.disableFormAlsNodig()
    this.zetOptioneleValidators()
  }

  ngOnDestroy() {
    if (this.cleanupEffect) {
      this.cleanupEffect.destroy()
    }
  }

  private getNewFileControl() {
    return this.formBuilder.control<File | null>(null, [extensieValidator(['csv']), aantalBestandenValidator(1)])
  }

  private disableFormAlsNodig() {
    if (
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK],
        actie: Actie.AANPASSEN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
        level: ToegangLevel.LANDELIJK,
        organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE],
        required: Required.ANY,
      }) &&
      this.isNieuw
    ) {
      this.editForm.enable()
    } else {
      this.editForm.disable()
    }
  }

  private zetOptioneleValidators() {
    if (this.visitatieTypeBe) {
      this.beoordelingseenheidCtrl.setValidators([Validators.required])
      this.fotorichtingCtrl.clearValidators()
    } else {
      this.beoordelingseenheidCtrl.clearValidators()
      this.fotorichtingCtrl.setValidators([Validators.required])
    }
    this.beoordelingseenheidCtrl.updateValueAndValidity()
    this.fotorichtingCtrl.updateValueAndValidity()
  }

  getBeoordelingseenheid(id?: number): OrganisatieDto | null {
    return this.beoordelingseenheden().find((org) => org.id === id) || null
  }

  sluitDialog() {
    this.dialogRef.close()
  }

  opslaan() {
    if (this.editForm.invalid) {
      return
    }

    const formValue = this.editForm.value
    const nieuweVisitatie = {
      id: this.visitatie.id,
      omschrijving: formValue.omschrijving,
      status: formValue.status,
      beoordelingseenheidId: formValue.beoordelingseenheid?.id,
      fotorichting: formValue.fotorichting,
    } as MammaVisitatieRequestDto

    const bestanden = new Map<MammaVisitatieOnderdeel, File>()

    const insteltechniekFile = this.editForm.get('insteltechniek')?.value as File | undefined
    if (insteltechniekFile) {
      bestanden.set(MammaVisitatieOnderdeel.INSTELTECHNIEK, insteltechniekFile)
    }

    const intervalcarcinomenFile = this.editForm.get('intervalcarcinomen')?.value as File | undefined
    if (intervalcarcinomenFile) {
      bestanden.set(MammaVisitatieOnderdeel.INTERVALCARCINOMEN, intervalcarcinomenFile)
    }

    const screenDetectedFile = this.editForm.get('screenDetected')?.value as File | undefined
    if (screenDetectedFile) {
      bestanden.set(MammaVisitatieOnderdeel.T2_PLUS_SCREEN_DETECTED, screenDetectedFile)
    }

    const verwijzingenFile = this.editForm.get('verwijzingen')?.value as File | undefined
    if (verwijzingenFile) {
      bestanden.set(MammaVisitatieOnderdeel.VERWIJZINGEN, verwijzingenFile)
    }

    const prothesesFile = this.editForm.get('protheses')?.value as File | undefined
    if (prothesesFile) {
      bestanden.set(MammaVisitatieOnderdeel.PROTHESES, prothesesFile)
    }

    const rapportageFile = this.editForm.get('rapportage')?.value as File | undefined
    const vragenlijstFile = this.editForm.get('vragenlijst')?.value as File | undefined

    this.visitatieService
      .saveVisitatie(nieuweVisitatie, bestanden, rapportageFile, vragenlijstFile)
      .pipe(take(1))
      .subscribe((response: MammaVisitatieResponseDto) => {
        if (response.meldingen.length) {
          this.notificationService.warning(response.meldingen.join(', '))
        }
        this.dialogRef.close(response.visitatie)
      })
  }

  protected getFotorichtingRadioItems() {
    return this.fotorichtingen.map(
      (richting) =>
        ({
          label: richting,
          value: richting,
        }) as DsRadiobuttonOption<string>,
    )
  }

  protected readonly Validators = Validators
}
