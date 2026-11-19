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
import { Component, inject } from '@angular/core'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { AbstractControl, FormBuilder, ReactiveFormsModule, ValidationErrors, Validators } from '@angular/forms'
import { addDays, addMonths, isBefore, startOfDay, subDays } from 'date-fns'
import { filter, switchMap, take } from 'rxjs'
import { DsButtonComponent, DsDatepickerComponent, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { ParameterService } from '@algemeen/services/parameter/parameter.service'
import { DateService } from '@shared/services/date/date.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { CervixUitstelDto } from '@shared/types/algemeen/dto/cervix-uitstel.dto'
import { ParameterKey } from '@shared/types/algemeen/enum/parameter-key'
import { CervixUitstelType, cervixUitstelTypeLabels } from '@shared/types/algemeen/enum/cervix-uitstel-type'
import { formatDate, formatNLDate, normaliseerNaarDate } from '@shared/utils/date-utils'
import { StringsService } from '@shared/strings'

export interface UitstelBaarmoederhalskankerDialogData {
  clientId: number
  uitstel: CervixUitstelDto
}

@Component({
  selector: 'app-uitstel-baarmoederhalskanker-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsDatepickerComponent, DsDropdownComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './uitstel-baarmoederhalskanker-dialog.component.html',
})
export class UitstelBaarmoederhalskankerDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly dialog = inject(Dialog)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly parameterService = inject(ParameterService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly data = inject<UitstelBaarmoederhalskankerDialogData>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()
  private readonly dagenUitstelBijZwangerschap = this.parameterService.getParameterWaarde(ParameterKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX, 0)
  protected readonly uitstelStrings = this.strings.algemeen.clientDossier.acties.uitstelBmhk

  protected get minDate() {
    return this.redenZwangerschap ? undefined : formatDate(addDays(DateService.nu(), 1))
  }

  protected get redenZwangerschap() {
    return this.redenControl.value === CervixUitstelType.ZWANGERSCHAP
  }

  protected readonly redenOpties = Object.values(CervixUitstelType).map((waarde) => ({ waarde, naam: cervixUitstelTypeLabels[waarde] }))

  private readonly redenControl = this.formBuilder.control<CervixUitstelType | null>(this.data.uitstel.uitstelType, Validators.required)

  protected readonly uitstelForm = this.formBuilder.group({
    reden: this.redenControl,
    datum: this.formBuilder.control<string | Date | null>(this.bepaalStandaardDatum(), [Validators.required, (control) => this.valideerUitstellenTotDatum(control)]),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  constructor() {
    this.redenControl.valueChanges.pipe(takeUntilDestroyed()).subscribe(() => this.uitstelForm.get('datum')?.updateValueAndValidity())
  }

  protected get datumLabel(): string {
    const reden = this.redenControl.value
    return reden ? this.uitstelStrings.datumLabels[reden] : ''
  }

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected uitstellen() {
    this.uitstelForm.markAllAsTouched()
    const { reden, datum: datumWaarde, notitie } = this.uitstelForm.getRawValue()
    const datum = normaliseerNaarDate(datumWaarde)
    if (this.uitstelForm.invalid || !reden || !datum) {
      return
    }

    this.dialog
      .open<boolean>(ConfirmationDialogComponent, { data: this.maakBevestigingData(datum), hasBackdrop: true })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.contactActiesService.vraagUitstelBaarmoederhalskankerAan(this.data.clientId, { uitstelType: reden, datum, notitie })),
      )
      .subscribe(() => {
        this.notificationService.success(this.uitstelStrings.notifications.succes)
        this.dialogRef.close(true)
      })
  }

  private maakBevestigingData(datum: Date) {
    const uitstellenTot = this.bepaalUitstellenTotDatum(datum)
    const regels = [this.uitstelStrings.bevestigingUitstelTot(formatNLDate(uitstellenTot))]
    if (this.data.uitstel.zasAanvraagWordtGeannuleerd) {
      regels.push(this.uitstelStrings.bevestigingZasGeannuleerd)
    }
    return {
      title: this.uitstelStrings.bevestigingTitel,
      body: `<ul class="met-bullets">${regels.map((regel) => `<li>${regel}</li>`).join('')}</ul>`,
      neeButtonTekst: this.strings.algemeen.dialogs.bevestiging.neeButtonTekst,
      jaButtonTekst: this.uitstelStrings.bevestigingJaButtonTekst,
    }
  }

  private bepaalStandaardDatum(): string | null {
    const { uitstelType, uitstellenTotDatum } = this.data.uitstel
    const datum = normaliseerNaarDate(uitstellenTotDatum)
    if (!datum) {
      return null
    }
    return formatDate(uitstelType === CervixUitstelType.ZWANGERSCHAP ? subDays(datum, this.dagenUitstelBijZwangerschap) : datum)
  }

  private bepaalUitstellenTotDatum(datum: Date): Date {
    return this.redenZwangerschap ? addDays(datum, this.dagenUitstelBijZwangerschap) : datum
  }

  private valideerUitstellenTotDatum(control: AbstractControl): ValidationErrors | null {
    const datum = normaliseerNaarDate(control.value)
    if (!datum) {
      return null
    }
    const morgen = startOfDay(addDays(DateService.nu(), 1))
    if (this.redenZwangerschap) {
      if (isBefore(startOfDay(this.bepaalUitstellenTotDatum(datum)), morgen)) {
        return { zwangerschapTeOud: this.uitstelStrings.errors.zwangerschapTeOud(this.dagenUitstelBijZwangerschap) }
      }
      if (isBefore(subDays(addMonths(morgen, 9), 1), datum)) {
        return { teVerInToekomst: this.uitstelStrings.errors.teVerInToekomst }
      }
    }
    return null
  }
}
