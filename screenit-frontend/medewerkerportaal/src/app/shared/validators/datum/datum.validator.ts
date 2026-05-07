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
import { addDays, addMonths, isAfter, isBefore, isValid, parse, startOfDay } from 'date-fns'
import { formatNLDate, isValideTijd, normaliseerNaarDate, parseDate } from '@shared/utils/date-utils'
import { TIME_FORMAT } from '@shared/constants'
import { NotificationService } from '@shared/services/notification/notification.service'

export const datumInVerledenValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty) {
    return null
  }

  const datum = normaliseerNaarDate(control.value)
  if (!datum) {
    return null
  }

  return startOfDay(datum) < startOfDay(new Date()) ? { datumInVerleden: 'De datum ligt in het verleden. Dit is niet toegestaan.' } : null
}

export const valideDatumValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty || !control.value) {
    return null
  }
  const datum = normaliseerNaarDate(control.value)
  return datum && isValid(datum) ? null : { valideDatum: 'De ingevoerde datum is niet valide.' }
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

    const validatieBericht = 'Datum en/of tijd liggen in het verleden. Dit is niet toegestaan.'
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
    return isBefore(ingevoerdeDatum, peilDatum) ? { minimumDatum: `De datum mag niet voor ${formatNLDate(peilDatum)} liggen` } : null
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
    return isAfter(ingevoerdeDatum, peilDatum) ? { maximumDatum: `De datum mag niet na ${formatNLDate(peilDatum)} liggen` } : null
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

    const validatieBericht = 'De starttijd moet voor de eindtijd liggen'
    const validatieError = eindDatum <= startDatum ? { startEindTijd: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, notificationService)
    return validatieError
  }
}

export const createStartEindDatumValidator = (
  startVeld: string,
  eindVeld: string,
  validatieBericht = 'De startdatum moet voor de einddatum liggen',
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
