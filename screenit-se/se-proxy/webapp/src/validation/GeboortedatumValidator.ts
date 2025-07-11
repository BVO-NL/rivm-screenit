/*-
 * ========================LICENSE_START=================================
 * se-proxy
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
import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"
import moment from "moment"

export class GeboortedatumValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isGeboortedatumValid(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateGeboortedatum(value, fieldLabel)

		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}

		return ""
	}

}

function isGeboortedatumValid(geboortedatum: string | undefined): boolean {
	if (geboortedatum) {
		if (geboortedatum.length === 10 && /[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}/.test(geboortedatum)) {
			const momentDate: moment.Moment = moment(geboortedatum, "YYYY-MM-DD")
			return momentDate.isAfter(moment().subtract(100, "years")) && momentDate.isBefore(moment())
		}
	}

	return false
}

function validateGeboortedatum(geboortedatum: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateGeboortedatumOngeldigError(geboortedatum, label, errors)
	return errors
}

function validateGeboortedatumOngeldigError(geboortedatum: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isGeboortedatumValid(geboortedatum)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}
