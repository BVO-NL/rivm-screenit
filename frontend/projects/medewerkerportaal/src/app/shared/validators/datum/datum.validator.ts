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
import { AbstractControl, FormControl, ValidationErrors, ValidatorFn } from '@angular/forms'
import { addDays, addMonths, addYears, isAfter, isBefore, isValid, parse, startOfDay } from 'date-fns'
import { formatNLDate, isValideTijd, normaliseerNaarDate, parseDate } from '@shared/utils/date-utils'
import { TIME_FORMAT } from '@shared/constants'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ALGEMEEN_STRINGS as algemeen } from '@shared/strings'

export const datumInVerledenValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty) {
    return null
  }

  const datum = normaliseerNaarDate(control.value)
  if (!datum) {
    return null
  }

  return startOfDay(datum) < startOfDay(new Date()) ? { datumInVerleden: algemeen.errors.datumInVerleden } : null
}

export const datumTijdInVerledenValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  const datumTijd = normaliseerNaarDate(control.value)
  if (!datumTijd || !isValid(datumTijd)) {
    return null
  }

  return isBefore(datumTijd, new Date()) ? { datumTijdInVerleden: algemeen.errors.datumTijdInVerleden } : null
}

export const valideDatumValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty || !control.value) {
    return null
  }
  const datum = normaliseerNaarDate(control.value)
  return datum && isValid(datum) ? null : { valideDatum: algemeen.errors.invalidDate }
}

export const createDatumTijdInVerledenValidator =
  (datumVeld: string, tijdVeld: string, notificationService?: NotificationService): ValidatorFn =>
  (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty) {
      return null
    }

    let datum = control.get(datumVeld)?.value
    const tijd = control.get(tijdVeld)?.value
    let invalid = true

    if (!datum) {
      return null
    }

    if (typeof datum === 'string') {
      datum = parseDate(datum)
    }
    invalid = startOfDay(datum) < startOfDay(new Date())

    if (tijd) {
      datum = parse(tijd, 'HH:mm', datum)
      invalid = datum < new Date()
    }

    const validatieBericht = algemeen.errors.datumEnOfTijdInVerleden
    const validatieError = invalid ? { datumTijdInVerleden: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, notificationService)
    return validatieError
  }

export const createMinimumDatumValidator =
  (datumCtrl: FormControl, plusDagen = 0): ValidatorFn =>
  (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty) {
      return null
    }

    const controleDatum = normaliseerNaarDate(datumCtrl.value)
    const ingevoerdeDatum = normaliseerNaarDate(control.value)
    if (controleDatum == null || ingevoerdeDatum == null) {
      return null
    }

    const peilDatum = addDays(controleDatum, plusDagen)
    return isBefore(ingevoerdeDatum, peilDatum) ? { minimumDatum: algemeen.errors.minimumDatum(formatNLDate(peilDatum)) } : null
  }

export const createMaximumDatumValidator =
  (datumCtrl: FormControl, maxMaanden: number): ValidatorFn =>
  (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty) {
      return null
    }

    const controleDatum = normaliseerNaarDate(datumCtrl.value)
    const ingevoerdeDatum = normaliseerNaarDate(control.value)
    if (controleDatum == null || ingevoerdeDatum == null) {
      return null
    }

    const peilDatum = addMonths(controleDatum, maxMaanden)
    return isAfter(ingevoerdeDatum, peilDatum) ? { maximumDatum: algemeen.errors.maximumDatum(formatNLDate(peilDatum)) } : null
  }

export const createStartEindTijdValidator = (startVeld: string, eindVeld: string, notificationService?: NotificationService): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const startTijd: string | undefined = control.get(startVeld)?.value
    const eindTijd: string | undefined = control.get(eindVeld)?.value

    if (!control.dirty || !startTijd || !eindTijd || !isValideTijd(startTijd) || !isValideTijd(eindTijd)) {
      return null
    }

    const startDatum = parse(startTijd, TIME_FORMAT, new Date())
    const eindDatum = parse(eindTijd, TIME_FORMAT, new Date())

    const validatieBericht = algemeen.errors.veldVoorVeld(algemeen.labels.starttijd, algemeen.labels.eindtijd)
    const validatieError = eindDatum <= startDatum ? { startEindTijd: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, notificationService)
    return validatieError
  }
}

export const createStartEindDatumValidator = (
  startVeld: string,
  eindVeld: string,
  validatieBericht = algemeen.errors.veldVoorVeld(algemeen.labels.begindatum, algemeen.labels.einddatum),
  notificationService?: NotificationService,
): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const startDatum: Date | undefined = control.get(startVeld)?.value
    const eindDatum: Date | undefined = control.get(eindVeld)?.value

    if (!control.dirty || !startDatum || !eindDatum) {
      return null
    }

    const validatieError = eindDatum < startDatum ? { startEindDatum: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, notificationService)
    return validatieError
  }
}

export const createEinddatumNaBegindatumValidator =
  (maxJaren = 1): ValidatorFn =>
  (group: AbstractControl): ValidationErrors | null => {
    const begindatum = group.get('begindatum')?.value
    const einddatumControl = group.get('einddatum')
    const einddatum = einddatumControl?.value

    const currentErrors = einddatumControl?.errors ?? {}
    const overigeErrors = { ...currentErrors }
    delete overigeErrors['einddatumNaBegindatum']
    delete overigeErrors['einddatumMaxJaren']

    if (begindatum && einddatum && new Date(einddatum) <= new Date(begindatum)) {
      overigeErrors['einddatumNaBegindatum'] = true
    }

    if (einddatum) {
      const maxDatum = addYears(new Date(), maxJaren)
      if (new Date(einddatum) > maxDatum) {
        overigeErrors['einddatumMaxJaren'] = true
      }
    }

    einddatumControl?.setErrors(Object.keys(overigeErrors).length ? overigeErrors : null)

    if (overigeErrors['einddatumNaBegindatum'] || overigeErrors['einddatumMaxJaren']) {
      return { einddatumOnjuist: true }
    }

    return null
  }

function toonOfLeegNotificaties(valide: boolean, error: string, notificationService?: NotificationService): void {
  if (!notificationService) {
    return
  }
  if (!valide) {
    notificationService.error(error)
  } else {
    notificationService.hide(error)
  }
}
