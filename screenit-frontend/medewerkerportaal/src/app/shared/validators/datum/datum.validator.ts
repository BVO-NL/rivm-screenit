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
import { AbstractControl, FormControl, ValidationErrors, ValidatorFn } from '@angular/forms'
import { addDays, addMonths, isAfter, isBefore, isValid, parse, startOfDay } from 'date-fns'
import { formatNLDate, parseDate } from '@shared/date-utils'
import { TIME_FORMAT } from '@shared/constants'
import { ToastService } from '@/toast/service/toast.service'

export const datumInVerledenValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty) {
    return null
  }
  let datum = control.value
  if (typeof datum === 'string') {
    datum = parseDate(datum)
  }
  return startOfDay(datum) < startOfDay(new Date()) ? { datumInVerleden: 'De datum ligt in het verleden. Dit is niet toegestaan.' } : null
}

export const valideDatumValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  if (!control.dirty || !control.value) {
    return null
  }
  let datum = control.value
  if (typeof datum === 'string') {
    datum = parseDate(datum)
  }
  return !isValid(datum) ? { valideDatum: 'De ingevoerde datum is niet valide.' } : null
}

export const createDatumTijdInVerledenValidator =
  (datumVeld: string, tijdVeld: string, toastService?: ToastService): ValidatorFn =>
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
    toonOfLeegNotificaties(validatieError == null, validatieBericht, toastService)
    return validatieError
  }

export const createMinimumDatumValidator =
  (datumCtrl: FormControl, plusDagen: number = 0): ValidatorFn =>
  (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty) {
      return null
    }

    const datumString: string = datumCtrl.value
    if (datumString == null) {
      return null
    }

    const datum = addDays(parseDate(datumString), plusDagen)
    return isBefore(parseDate(control.value), datum) ? { minimumDatum: `De datum mag niet voor ${formatNLDate(datum)} liggen` } : null
  }

export const createMaximumDatumValidator =
  (datumCtrl: FormControl, maxMaanden: number): ValidatorFn =>
  (control: AbstractControl): ValidationErrors | null => {
    if (!control.dirty) {
      return null
    }

    const datumString: string = datumCtrl.value
    if (datumString == null) {
      return null
    }

    const datum = addMonths(parseDate(datumString), maxMaanden)
    return isAfter(parseDate(control.value), datum) ? { maximumDatum: `De datum mag niet na ${formatNLDate(datum)} liggen` } : null
  }

export const createStartEindTijdValidator = (startVeld: string, eindVeld: string, toastService?: ToastService): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const startTijd: string | undefined = control.get(startVeld)?.value
    const eindTijd: string | undefined = control.get(eindVeld)?.value

    if (!control.dirty || !startTijd || !eindTijd) {
      return null
    }

    const startDatum = parse(startTijd, TIME_FORMAT, new Date())
    const eindDatum = parse(eindTijd, TIME_FORMAT, new Date())

    const validatieBericht = 'De starttijd moet voor de eindtijd liggen'
    const validatieError = eindDatum <= startDatum ? { startEindTijd: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, toastService)
    return validatieError
  }
}

export const createStartEindDatumValidator = (
  startVeld: string,
  eindVeld: string,
  validatieBericht: string = 'De startdatum moet voor de einddatum liggen',
  toastService?: ToastService,
): ValidatorFn => {
  return (control: AbstractControl): ValidationErrors | null => {
    const startDatumString: string | undefined = control.get(startVeld)?.value
    const eindDatumString: string | undefined = control.get(eindVeld)?.value

    if (!control.dirty || !startDatumString || !eindDatumString) {
      return null
    }

    const startDatum = startOfDay(parseDate(startDatumString))
    const eindDatum = startOfDay(parseDate(eindDatumString))

    const validatieError = eindDatum < startDatum ? { startEindDatum: validatieBericht } : null
    toonOfLeegNotificaties(validatieError == null, validatieBericht, toastService)
    return validatieError
  }
}

function toonOfLeegNotificaties(valide: boolean, error: string, toastService?: ToastService): void {
  if (!toastService) {
    return
  }
  if (!valide) {
    toastService.error(error)
  } else {
    toastService.hide(error)
  }
}
