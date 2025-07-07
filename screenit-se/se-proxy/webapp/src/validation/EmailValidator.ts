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

export class EmailValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isEmailadresValideOfLeeg(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateEmailadres(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}
}

function isEmailadresValideOfLeeg(email: string | undefined, verplicht = false): boolean {
	if (!email) {
		return !verplicht
	}
	return email.length <= 100 && /^[_A-Za-z0-9-]+(\.[_A-Za-z0-9-]+)*@[a-zA-Z0-9]+([.-][a-zA-Z0-9]+)*\.[a-zA-Z]{2,}$/.test(email)
}

function validateEmailadres(email: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateEmailadresOngeldigError(email, label, errors)
	return errors
}

function validateEmailadresOngeldigError(email: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isEmailadresValideOfLeeg(email)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}
