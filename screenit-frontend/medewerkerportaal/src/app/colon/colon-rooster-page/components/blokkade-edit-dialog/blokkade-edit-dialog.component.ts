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
import { Component, inject, Signal } from '@angular/core'

import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { ClrButtonModule, ClrCheckboxModule, ClrComboboxModule, ClrDatepickerModule, ClrInputModule, ClrModalModule } from '@clr/angular'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { addMinutes, endOfDay, format, parse, startOfDay } from 'date-fns'
import { ColonRoosterInstellingen } from '@shared/types/colon/colon-rooster-instellingen'
import { catchError, filter, iif, Observable, switchMap, take, throwError } from 'rxjs'
import { tijdStappenValidator } from '@shared/validators/tijd-stappen/tijd-stappen.validator'
import { DATE_FORMAT, NL_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { createDatumTijdInVerledenValidator, createStartEindTijdValidator, valideDatumValidator } from '@shared/validators/datum/datum.validator'
import { formatDateAsISO, formatDateStringAsTime, formatNLDate, formatTimeAsISO, parseDate } from '@shared/date-utils'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ColonService } from '@/colon/services/colon.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/autorisatie/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { ToastService } from '@/toast/service/toast.service'
import { BulkAanmakenFormComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-form/bulk-aanmaken-form.component'
import { ColonHerhalingsfrequentie } from '@shared/types/colon/colon-herhaling-frequentie'
import { HttpErrorResponse } from '@angular/common/http'
import { BulkAanmakenBevestigingsPopupComponent } from '@/colon/colon-rooster-page/components/bulk-aanmaken-bevestigings-popup/bulk-aanmaken-bevestigings-popup.component'

@Component({
  selector: 'app-blokkade-edit-dialog',
  imports: [
    ClrModalModule,
    ClrInputModule,
    ClrDatepickerModule,
    ClrButtonModule,
    ClrComboboxModule,
    ReactiveFormsModule,
    ClrCheckboxModule,
    BaseDialogComponent,
    BulkAanmakenFormComponent,
  ],
  templateUrl: './blokkade-edit-dialog.component.html',
  styleUrls: ['./blokkade-edit-dialog.component.scss'],
})
export class BlokkadeEditDialogComponent {
  private formBuilder: FormBuilder = inject(FormBuilder)
  private roosterService: RoosterService = inject(RoosterService)
  private blokkade: ColonBlokkade = inject(DIALOG_DATA)
  private dialogRef = inject(DialogRef)
  private dialog: Dialog = inject(Dialog)
  private colonService: ColonService = inject(ColonService)
  private autorisatieService: AutorisatieService = inject(AutorisatieService)
  private toastService: ToastService = inject(ToastService)
  private dialogService = inject(Dialog)

  now = format(new Date(), DATE_FORMAT)
  instellingen$: Signal<ColonRoosterInstellingen> = this.roosterService.instellingen
  kamers$: Signal<ColonKamer[]> = this.roosterService.kamers
  blokkadeForm: FormGroup = this.formBuilder.group(
    {
      id: null,
      kamer: [{ value: null, disabled: !this.isNieuw }, [Validators.required]],
      alleKamers: false,
      vanaf: ['00:00', { validators: [Validators.required, tijdStappenValidator], updateOn: 'blur' }],
      tot: ['23:59', { validators: [Validators.required], updateOn: 'blur' }],
      heleDag: true,
      datum: [
        format(this.instellingen$().geprognosticeerdeVanafDatum, NL_DATE_FORMAT),
        {
          validators: [Validators.required, valideDatumValidator],
          updateOn: 'blur',
        },
      ],
      omschrijving: [null, Validators.required],
      herhaling: null,
    },
    { validators: [createStartEindTijdValidator('vanaf', 'tot', this.toastService), createDatumTijdInVerledenValidator('datum', 'vanaf', this.toastService)] },
  )
  verwijderenToegestaan = false
  bewerkenToegestaan = false

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

  get datumCtrl(): FormControl {
    return this.blokkadeForm.get('datum') as FormControl
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
        datum: formatNLDate(new Date(this.blokkade.vanaf)),
        kamer: this.kamers$().find((kamer) => kamer.id === this.blokkade.kamerId),
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
    const eindDate: Date = addMinutes(startDate, this.instellingen$().duurAfspraakInMinuten)
    const eindTijd = format(eindDate, TIME_FORMAT)
    this.eindTijdCtrl.setValue(eindTijd, { emitEvent: false })
  }

  cancel() {
    this.dialogRef.close()
  }

  save(alleenValidatie = true) {
    if (this.blokkadeForm.invalid) {
      return
    }

    const formValue = this.blokkadeForm.getRawValue()
    const datum = parseDate(formValue.datum)
    const blokkade = {
      id: formValue.id,
      kamerId: formValue.kamer?.id,
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

    if (this.isNieuw && formValue.herhaling && formValue.herhaling.frequentie !== ColonHerhalingsfrequentie.GEEN_HERHALING) {
      this.handleBulkSave(blokkade, alleenValidatie)
    } else {
      this.handleSingleSave(blokkade, false)
    }
  }

  private handleBulkSave(blokkade: ColonBlokkade, alleenValidatie: boolean) {
    this.roosterService
      .createBlokkades(blokkade, alleenValidatie)
      .pipe(
        take(1),
        catchError((response: HttpErrorResponse) => {
          if (response.status === 422 && response.error?.type === 'bulk') {
            return this.dialogService.open(BulkAanmakenBevestigingsPopupComponent, { data: { exceptions: response.error.exceptions, typeTijdslot: 'blokkades' } }).closed.pipe(
              take(1),
              filter((response: unknown) => response === true),
            )
          }
          return throwError(() => response)
        }),
        switchMap(() => this.roosterService.createBlokkades(blokkade, false)),
      )
      .subscribe(() => {
        this.dialogRef.close()
      })
  }

  private handleSingleSave(blokkade: ColonBlokkade, alleenValidatie: boolean) {
    this.saveOrUpdate(blokkade, alleenValidatie).subscribe(() => {
      this.dialogRef.close()
    })
  }

  private saveOrUpdate(blokkade: ColonBlokkade, alleenValidatie: boolean): Observable<unknown> {
    return iif(() => this.isNieuw, this.roosterService.createBlokkades(blokkade, alleenValidatie), this.roosterService.updateBlokkade(blokkade, alleenValidatie))
  }

  confirmDelete() {
    const formValue = this.blokkadeForm.getRawValue()
    const intakelocatie = this.colonService.intakelocatie
    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Weet u zeker dat u deze blokkade wilt verwijderen?',
          body: `${intakelocatie()?.naam} - ${formValue.kamer?.naam}: ${formValue.datum} van ${formValue.vanaf} tot ${formValue.tot}`,
        },
      })
      .closed.pipe(take(1))
      .subscribe((remove: unknown) => {
        if (remove === true) {
          this.deleteBlokkade()
        }
      })
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
