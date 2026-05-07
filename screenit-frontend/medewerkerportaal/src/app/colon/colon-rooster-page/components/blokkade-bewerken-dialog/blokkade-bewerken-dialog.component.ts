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

import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { addMinutes, endOfDay, format, isValid, parse, startOfDay } from 'date-fns'
import { catchError, filter, Observable, switchMap, take, throwError } from 'rxjs'
import { tijdStappenValidator } from '@shared/validators/tijd-stappen/tijd-stappen.validator'
import { DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { createDatumTijdInVerledenValidator, createStartEindTijdValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { formatDateAsISO, formatDateStringAsTime, formatNLDate, formatTimeAsISO } from '@shared/utils/date-utils'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ColonService } from '@/colon/services/colon.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@/shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { BulkAanmakenFormComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-form/bulk-aanmaken-form.component'
import { ColonHerhalingsfrequentie } from '@shared/types/colon/colon-herhaling-frequentie'
import { HttpErrorResponse } from '@angular/common/http'
import { BulkAanmakenBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-bevestigings-popup/bulk-aanmaken-bevestigings-popup.component'
import { DsButtonComponent, DsCheckboxComponent, DsDatepickerComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { KamerSelectorComponent } from '@/colon/colon-rooster-page/components/kamer-selector/kamer-selector.component'
import { bepaalFoutmeldingBijOpslaanTijdslot } from '@/colon/utils/colon-utils'
import { ColonTijdslotType } from '@shared/types/colon/colon-tijdslot-type'
import { TimepickerComponent } from '@shared/components/timepicker/timepicker.component'

@Component({
  selector: 'app-blokkade-bewerken-dialog',
  imports: [
    ReactiveFormsModule,
    BaseDialogComponent,
    DsDatepickerComponent,
    DsInputComponent,
    DsCheckboxComponent,
    DsButtonComponent,
    BulkAanmakenFormComponent,
    KamerSelectorComponent,
    TimepickerComponent,
  ],
  templateUrl: './blokkade-bewerken-dialog.component.html',
  styleUrls: ['./blokkade-bewerken-dialog.component.scss'],
})
export class BlokkadeBewerkenDialogComponent {
  minDatum = format(new Date(), DATE_FORMAT)
  verwijderenToegestaan = false
  bewerkenToegestaan = false
  private readonly formBuilder: FormBuilder = inject(FormBuilder)
  private readonly roosterService: RoosterService = inject(RoosterService)
  private readonly blokkade: ColonBlokkade = inject(DIALOG_DATA)
  private readonly dialogRef = inject(DialogRef)
  private readonly dialog: Dialog = inject(Dialog)
  private readonly colonService: ColonService = inject(ColonService)
  private readonly autorisatieService: AutorisatieService = inject(AutorisatieService)
  private readonly notificationService: NotificationService = inject(NotificationService)
  private readonly dialogService = inject(Dialog)

  blokkadeForm: FormGroup = this.formBuilder.group(
    {
      id: null,
      kamer: [{ value: null, disabled: !this.isNieuw }, [Validators.required]],
      alleKamers: false,
      vanaf: ['00:00', { validators: [Validators.required, tijdStappenValidator], updateOn: 'blur' }],
      tot: ['23:59', { validators: [Validators.required], updateOn: 'blur' }],
      heleDag: true,
      datum: [
        this.roosterService.instellingen().geprognosticeerdeVanafDatum,
        {
          validators: [Validators.required, valideDatumValidator],
        },
      ],
      omschrijving: [null, Validators.required],
      herhaling: null,
    },
    { validators: [createStartEindTijdValidator('vanaf', 'tot', this.notificationService), createDatumTijdInVerledenValidator('datum', 'vanaf', this.notificationService)] },
  )

  get isNieuw(): boolean {
    return this.blokkade == null
  }

  get title(): string {
    if (this.bewerkenToegestaan) {
      return this.isNieuw ? 'Aanmaken blokkade(s)' : 'Bewerken blokkade'
    }
    return 'Blokkade'
  }

  get startTijdCtrl(): FormControl {
    return this.blokkadeForm.get('vanaf') as FormControl
  }

  get eindTijdCtrl(): FormControl {
    return this.blokkadeForm.get('tot') as FormControl
  }

  get heleDagCtrl(): FormControl {
    return this.blokkadeForm.get('heleDag') as FormControl
  }

  get kamerCtrl(): FormControl {
    return this.blokkadeForm.get('kamer') as FormControl
  }

  get alleKamersCtrl(): FormControl {
    return this.blokkadeForm.get('alleKamers') as FormControl
  }

  constructor() {
    let isVerleden = false
    if (this.blokkade) {
      this.blokkadeForm.patchValue({
        ...this.blokkade,
        vanaf: formatDateStringAsTime(this.blokkade.vanaf),
        tot: formatDateStringAsTime(this.blokkade.tot),
        heleDag: this.blokkade.tot == formatDateAsISO(endOfDay(new Date(this.blokkade.tot))),
        datum: new Date(this.blokkade.vanaf),
        kamer: this.blokkade.kamerId,
        alleKamers: false,
      })
      isVerleden = new Date(this.blokkade.vanaf) < new Date()
    }

    if (this.heleDagCtrl.value) {
      this.startTijdCtrl.disable()
      this.eindTijdCtrl.disable()
    }

    this.verwijderenToegestaan =
      !this.isNieuw &&
      !isVerleden &&
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_LOCATIE_ROOSTER],
        actie: Actie.VERWIJDEREN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
        level: ToegangLevel.ORGANISATIE,
        organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
        required: Required.ANY,
      })

    this.bewerkenToegestaan =
      (this.isNieuw || (!this.isNieuw && !isVerleden)) &&
      this.autorisatieService.isToegestaan({
        recht: [Recht.MEDEWERKER_LOCATIE_ROOSTER],
        actie: Actie.AANPASSEN,
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
        level: ToegangLevel.ORGANISATIE,
        organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
        required: Required.ANY,
      })

    if (!this.bewerkenToegestaan) {
      this.blokkadeForm.disable()
    }

    this.setFormChangeHandlers()
  }

  cancel() {
    this.dialogRef.close()
  }

  save(alleenValidatie = true) {
    if (this.blokkadeForm.invalid) {
      return
    }

    const formValue = this.blokkadeForm.getRawValue()
    const datum = startOfDay(formValue.datum)
    const blokkade = {
      id: formValue.id,
      kamerId: formValue.kamer,
      alleKamers: formValue.alleKamers,
      omschrijving: formValue.omschrijving,
      vanaf: formValue.vanaf,
      tot: formValue.tot,
      herhaling: formValue.herhaling,
    }

    if (formValue.heleDag) {
      blokkade.vanaf = formatDateAsISO(startOfDay(datum))
      blokkade.tot = formatDateAsISO(endOfDay(datum))
    } else {
      blokkade.vanaf = formatTimeAsISO(formValue.vanaf, datum)
      blokkade.tot = formatTimeAsISO(formValue.tot, datum)
    }

    const bulkAanmaken = this.isNieuw && formValue.herhaling != null && formValue.herhaling.frequentie !== ColonHerhalingsfrequentie.GEEN_HERHALING
    const observable = bulkAanmaken ? this.handleBulkSave(blokkade, alleenValidatie) : this.handleSingleSave(blokkade)
    observable.subscribe({
      next: () => this.dialogRef.close(),
      error: (err: HttpErrorResponse) => {
        const message = bepaalFoutmeldingBijOpslaanTijdslot(err, ColonTijdslotType.BLOKKADE)
        this.notificationService.error(message)
      },
    })
  }

  confirmDelete() {
    const formValue = this.blokkadeForm.getRawValue()
    const intakelocatie = this.colonService.intakelocatie
    const kamer = this.roosterService.kamers().find((k) => k.id === formValue.kamer)
    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Weet u zeker dat u deze blokkade wilt verwijderen?',
          body: `${intakelocatie()?.naam} - ${kamer?.naam}: ${formatNLDate(formValue.datum)} van ${formValue.vanaf} tot ${formValue.tot}`,
        },
      })
      .closed.pipe(take(1))
      .subscribe((remove: unknown) => {
        if (remove === true) {
          this.deleteBlokkade()
        }
      })
  }

  private setFormChangeHandlers() {
    this.heleDagCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((enabled: boolean) => {
      if (enabled) {
        this.startTijdCtrl.setValue('00:00', { emitEvent: true })
        this.eindTijdCtrl.setValue('23:59', { emitEvent: true })
        this.startTijdCtrl.disable()
        this.eindTijdCtrl.disable()
      } else {
        this.startTijdCtrl.reset(null, { emitEvent: true })
        this.eindTijdCtrl.reset(null, { emitEvent: true })
        this.startTijdCtrl.enable()
        this.eindTijdCtrl.enable()
      }
    })

    this.alleKamersCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((enabled: boolean) => {
      enabled ? this.kamerCtrl.disable() : this.kamerCtrl.enable()
    })

    this.startTijdCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter((value: string) => value != null && value != '' && !this.heleDagCtrl.value),
      )
      .subscribe(() => this.berekenEindTijd())
  }

  private berekenEindTijd(): void {
    const startDate: Date = parse(this.startTijdCtrl.value, TIME_FORMAT, new Date())
    if (!isValid(startDate)) {
      return
    }
    const eindDate: Date = addMinutes(startDate, this.roosterService.instellingen().duurAfspraakInMinuten)
    const eindTijd = format(eindDate, TIME_FORMAT)
    this.eindTijdCtrl.setValue(eindTijd, { emitEvent: false })
  }

  private handleBulkSave(blokkade: ColonBlokkade, alleenValidatie: boolean) {
    return this.roosterService.createBlokkades(blokkade, alleenValidatie).pipe(
      take(1),
      catchError((response: HttpErrorResponse) => {
        if (response.status === 422 && response.error?.type === 'bulk') {
          return this.dialogService
            .open(BulkAanmakenBevestigingsPopupComponent, {
              data: {
                exceptions: response.error.exceptions,
                typeTijdslot: 'blokkades',
              },
            })
            .closed.pipe(
              take(1),
              filter((result) => result === true),
            )
        }

        return throwError(() => response)
      }),
      switchMap(() => this.roosterService.createBlokkades(blokkade, false)),
    )
  }

  private handleSingleSave(blokkade: ColonBlokkade): Observable<unknown> {
    return this.isNieuw ? this.roosterService.createBlokkades(blokkade, false) : this.roosterService.updateBlokkade(blokkade, false)
  }

  private deleteBlokkade() {
    this.roosterService
      .deleteBlokkade(Number(this.blokkade.id))
      .pipe(take(1))
      .subscribe(() => {
        this.dialogRef.close()
      })
  }
}
