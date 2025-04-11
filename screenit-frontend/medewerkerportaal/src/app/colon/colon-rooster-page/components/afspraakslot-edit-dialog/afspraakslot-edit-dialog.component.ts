/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Component, inject, LOCALE_ID, Signal } from '@angular/core'

import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { ClrButtonModule, ClrComboboxModule, ClrDatepickerModule, ClrInputModule, ClrModalModule } from '@clr/angular'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'
import { addMinutes, differenceInMinutes, format, parse } from 'date-fns'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonRoosterInstellingen } from '@shared/types/colon/colon-rooster-instellingen'
import { catchError, filter, iif, merge, Observable, switchMap, take, throwError } from 'rxjs'
import { tijdStappenValidator } from '@shared/validators/tijd-stappen/tijd-stappen.validator'
import { createMaxAantalBlokkenValidator } from '@shared/validators/max-aantal-blokken/max-aantal-blokken.validator'
import { TIME_FORMAT } from '@shared/constants'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/autorisatie/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { Recht } from '@shared/types/autorisatie/recht'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ColonTijdslotStatus } from '@shared/types/colon/colon-tijdslot-status'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { formatDate, formatDateStringAsTime, formatNLDate, formatTimeAsISO, parseDate } from '@shared/date-utils'
import { positiveIntegerValidator } from '@shared/validators/common-validators'
import { createDatumTijdInVerledenValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { ToastService } from '@/toast/service/toast.service'
import { HttpErrorResponse } from '@angular/common/http'
import { BulkAanmakenFormComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-form/bulk-aanmaken-form.component'
import { BulkAanmakenBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-bevestigings-popup/bulk-aanmaken-bevestigings-popup.component'
import { ZachteBeperkingBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/zachte-beperking-bevestigings-popup/zachte-beperking-bevestigings-popup.component'
import { ColonHerhalingsfrequentie } from '@shared/types/colon/colon-herhaling-frequentie'
import { ColonRoosterBeperking } from '@shared/types/colon/colon-rooster-beperking'

@Component({
  selector: 'app-afspraakslot-edit-dialog',
  imports: [ClrModalModule, ClrInputModule, ClrDatepickerModule, ClrButtonModule, ClrComboboxModule, ReactiveFormsModule, BaseDialogComponent, BulkAanmakenFormComponent],
  templateUrl: './afspraakslot-edit-dialog.component.html',
  providers: [{ provide: LOCALE_ID, useValue: 'nl' }],
})
export class AfspraakslotEditDialogComponent {
  private formBuilder: FormBuilder = inject(FormBuilder)
  private roosterService: RoosterService = inject(RoosterService)
  private afspraakslot: ColonAfspraakslot = inject(DIALOG_DATA)
  private dialogRef = inject(DialogRef)
  private dialogService = inject(Dialog)
  private autorisatieService: AutorisatieService = inject(AutorisatieService)
  private toastService: ToastService = inject(ToastService)

  now = formatDate(new Date())
  instellingen$: Signal<ColonRoosterInstellingen> = this.roosterService.instellingen
  kamers$: Signal<ColonKamer[]> = this.roosterService.kamers
  bewerkenToegestaan = false
  verwijderenToegestaan = false
  afspraakslotForm: FormGroup = this.formBuilder.group(
    {
      id: null,
      kamer: [{ value: null, disabled: !this.isNieuw }, Validators.required],
      vanaf: [null, { validators: [Validators.required, tijdStappenValidator], updateOn: 'blur' }],
      tot: [{ value: null, disabled: true }],
      datum: [
        formatNLDate(this.instellingen$().geprognosticeerdeVanafDatum),
        {
          validators: [Validators.required, valideDatumValidator],
          updateOn: 'blur',
        },
      ],
      aantalBlokken: [this.berekenAantalBlokken(this.afspraakslot), { validators: [Validators.required, Validators.min(1), positiveIntegerValidator], updateOn: 'blur' }],
      herhaling: null,
    },
    {
      validators: [
        createMaxAantalBlokkenValidator(this.instellingen$().duurAfspraakInMinuten, this.toastService),
        createDatumTijdInVerledenValidator('datum', 'vanaf', this.toastService),
      ],
    },
  )

  get isNieuw(): boolean {
    return this.afspraakslot?.id == null
  }

  get title(): string {
    if (this.bewerkenToegestaan) {
      return this.isNieuw ? 'Aanmaken afspraakslot(s)' : 'Bewerken afspraakslot'
    }
    return 'Afspraakslot'
  }

  get aantalBlokkenCtrl(): FormControl {
    return this.afspraakslotForm.get('aantalBlokken') as FormControl
  }

  get startTijdCtrl(): FormControl {
    return this.afspraakslotForm.get('vanaf') as FormControl
  }

  get eindTijdCtrl(): FormControl {
    return this.afspraakslotForm.get('tot') as FormControl
  }

  get datumCtrl(): FormControl {
    return this.afspraakslotForm.get('datum') as FormControl
  }

  constructor() {
    if (this.afspraakslot) {
      this.afspraakslotForm.patchValue(
        {
          ...this.afspraakslot,
          vanaf: formatDateStringAsTime(this.afspraakslot.vanaf),
          tot: formatDateStringAsTime(this.afspraakslot.tot),
          datum: formatNLDate(new Date(this.afspraakslot.vanaf)),
          kamer: this.kamers$().find((kamer: ColonKamer) => kamer.id === this.afspraakslot.kamerId),
        },
        { onlySelf: true, emitEvent: false },
      )
      this.berekenEindTijd()
    }

    this.verwijderenToegestaan =
      !this.isNieuw &&
      this.autorisatieService.isToegestaan({
        recht: [Recht.GEBRUIKER_LOCATIE_ROOSTER],
        actie: Actie.VERWIJDEREN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
        level: ToegangLevel.INSTELLING,
        organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
        required: Required.ANY,
      }) &&
      this.afspraakslot.status != ColonTijdslotStatus.GEBRUIKT_VOOR_CAPACITEIT

    this.bewerkenToegestaan =
      this.autorisatieService.isToegestaan({
        recht: [Recht.GEBRUIKER_LOCATIE_ROOSTER],
        actie: Actie.AANPASSEN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
        level: ToegangLevel.INSTELLING,
        organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
        required: Required.ANY,
      }) &&
      (this.isNieuw || this.afspraakslot?.status !== ColonTijdslotStatus.INTAKE_GEPLAND)

    if (!this.bewerkenToegestaan) {
      this.afspraakslotForm.disable()
    }

    this.setFormChangeHandlers()
  }

  clearNotifications() {
    this.toastService.clear()
  }

  private berekenAantalBlokken(afspraakSlot: ColonAfspraakslot) {
    if (!afspraakSlot?.vanaf || !afspraakSlot?.tot) {
      return 1
    }
    const startDatumTijd: Date = new Date(afspraakSlot.vanaf)
    const eindDatumTijd: Date = new Date(afspraakSlot.tot)
    return Math.ceil(differenceInMinutes(eindDatumTijd, startDatumTijd) / this.instellingen$().duurAfspraakInMinuten)
  }

  private setFormChangeHandlers() {
    merge(this.aantalBlokkenCtrl.valueChanges, this.startTijdCtrl.valueChanges, this.datumCtrl.valueChanges)
      .pipe(
        takeUntilDestroyed(),
        filter(() => {
          const startTijd = this.startTijdCtrl.value
          const aantalBlokken = this.aantalBlokkenCtrl.value
          return startTijd && aantalBlokken && (this.datumCtrl.dirty || this.startTijdCtrl.dirty || this.aantalBlokkenCtrl.dirty)
        }),
      )
      .subscribe(() => {
        this.clearNotifications()
        this.berekenEindTijd()
      })
  }

  private berekenEindTijd(): void {
    const startDate: Date = parse(this.startTijdCtrl.value, TIME_FORMAT, new Date())
    const eindDate: Date = addMinutes(startDate, this.instellingen$().duurAfspraakInMinuten * this.aantalBlokkenCtrl.value)
    const eindTijd = format(eindDate, TIME_FORMAT)
    this.eindTijdCtrl.setValue(eindTijd, { emitEvent: false })
  }

  cancel() {
    this.dialogRef.close()
  }

  save(alleenValidatie = true) {
    if (this.afspraakslotForm.invalid) {
      return
    }

    const formValue = this.afspraakslotForm.getRawValue()
    const datum = parseDate(formValue.datum)
    const afspraakslot = {
      id: formValue.id,
      kamerId: formValue.kamer.id,
      vanaf: formatTimeAsISO(formValue.vanaf, datum),
      tot: formatTimeAsISO(formValue.tot, datum),
      aantalBlokken: formValue.aantalBlokken,
      capaciteitMeeBepaald: false,
      herhaling: formValue.herhaling,
    }

    if (this.isNieuw && formValue.herhaling && formValue.herhaling.frequentie !== ColonHerhalingsfrequentie.GEEN_HERHALING) {
      this.handleBulkSave(afspraakslot, alleenValidatie)
    } else {
      this.handleSingleSave(afspraakslot, alleenValidatie)
    }
  }

  private handleBulkSave(afspraakslot: ColonAfspraakslot, alleenValidatie: boolean) {
    this.roosterService
      .createAfspraakslots(afspraakslot, alleenValidatie)
      .pipe(
        take(1),
        catchError((response: HttpErrorResponse) => {
          if (response.status === 422 && response.error?.type === 'bulk') {
            return this.dialogService.open(BulkAanmakenBevestigingsPopupComponent, { data: { exceptions: response.error.exceptions, typeTijdslot: 'afspraakslots' } }).closed.pipe(
              take(1),
              filter((response: unknown) => response === true),
            )
          }
          return throwError(() => response)
        }),
        switchMap(() => this.roosterService.createAfspraakslots(afspraakslot, false)),
      )
      .subscribe(() => {
        this.dialogRef.close()
      })
  }

  private handleSingleSave(afspraakslot: ColonAfspraakslot, alleenValidatie: boolean) {
    this.saveOrUpdate(afspraakslot, alleenValidatie)
      .pipe(
        take(1),
        catchError((response: HttpErrorResponse) => {
          if (response.status === 422 && response.error?.beperkingType === ColonRoosterBeperking.ZACHT) {
            return this.dialogService
              .open(ZachteBeperkingBevestigingsPopupComponent, {
                data: {
                  messages: response.error.messages,
                },
              })
              .closed.pipe(
                take(1),
                filter((response: unknown) => response === true),
              )
          }
          return throwError(() => response)
        }),
        switchMap(() => this.saveOrUpdate(afspraakslot, false)),
      )
      .subscribe(() => {
        this.dialogRef.close()
      })
  }

  private saveOrUpdate(afspraakslot: ColonAfspraakslot, alleenValidatie: boolean): Observable<unknown> {
    return iif(() => this.isNieuw, this.roosterService.createAfspraakslots(afspraakslot, alleenValidatie), this.roosterService.updateAfspraakslot(afspraakslot, alleenValidatie))
  }

  confirmDeleteSlot() {
    const formValue = this.afspraakslotForm.getRawValue()

    this.dialogService
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Weet u zeker dat u dit afspraakslot wilt verwijderen?',
          body: `${formValue.kamer.naam}: ${formValue.datum} van ${formValue.vanaf} tot ${formValue.tot}`,
        },
        minWidth: '300px',
      })
      .closed.pipe(take(1))
      .subscribe((res: unknown) => {
        if (res === true) {
          this.deleteSlot()
        }
      })
  }

  deleteSlot() {
    this.roosterService
      .deleteAfspraakslot(Number(this.afspraakslot.id))
      .pipe(take(1))
      .subscribe(() => {
        this.dialogRef.close()
      })
  }
}
