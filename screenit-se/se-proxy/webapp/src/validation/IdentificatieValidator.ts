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
import type {Identificatiesoort} from "../datatypes/Afspraak"
import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"
import type {Validation} from "./Validation"

export type IdentificatieType = {
	identificatienummer?: string;
	identificatiesoort?: Identificatiesoort;
};

export class IdentificatieValidator<T extends IdentificatieType | undefined> implements Validation<T> {

	isValid(value?: IdentificatieType): boolean {
		if (value) {
			const errors = validateIdentificatieNummer(value.identificatienummer ? value.identificatienummer : "", value.identificatiesoort)
			return errors.length === 0
		}

		return false
	}

	getErrorMessage(value: IdentificatieType | undefined, fieldLabel: string): string {
		if (value) {
			const errors = validateIdentificatieNummer(value.identificatienummer ? value.identificatienummer : "", value.identificatiesoort)
			if (errors.length > 0) {
				return getErrorMessage(errors[0])
			}
		}
		return ""
	}

}

const identificatiesoortValidatieNaam = (soort: Identificatiesoort): string => {
	switch (soort) {
		case "PASPOORT":
			return "Nederlands paspoortnummer"
		case "RIJBEWIJS":
			return "Nederlands rijbewijsnummer"
		case "IDENTITEITSKAART":
			return "Nederlands identiteitskaartnummer"
		case "OVERIG":
			return "Overig tekst"
		default:
			return "Identificatienummer"
	}
}

const validateRequiredError = (value: string | undefined, errorLabel: string, errors: ValidationError[]): void => {
	if (!value || value.trim() === "") {
		errors.push({
			type: "verplicht",
			label: errorLabel,
		})
	}
}

const validateIdentificatieNummerOngeldigError = (identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined, errorLabel: string, errors: ValidationError[]): void => {
	let isValid = false
	switch (identificatiesoort) {
		case "PASPOORT":
		case "IDENTITEITSKAART":
			isValid = !!identificatienummer && /[A-NP-Za-np-z]{2}[A-NP-Za-np-z0-9]{6}[0-9]{1}/.test(identificatienummer)
			break
		case "RIJBEWIJS":
			isValid = !!identificatienummer && /[0-9]{10}/.test(identificatienummer)
			break
		case "OVERIG":
			isValid = true
			break
		default:
			break
	}

	if (!isValid) {
		errors.push({
			type: "ongeldig",
			label: errorLabel,
		})
	}
}

export function validateIdentificatieNummer(identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined): ValidationError[] {
	const errorLabel = identificatiesoort ? identificatiesoortValidatieNaam(identificatiesoort) : "Identificatienummer"
	const errors: ValidationError[] = []
	validateRequiredError(identificatienummer, errorLabel, errors)
	validateIdentificatieNummerOngeldigError(identificatienummer, identificatiesoort, errorLabel, errors)
	return errors
}

export function isValidIdentificatieNummer(identificatienummer: string | undefined, identificatiesoort: Identificatiesoort | undefined): boolean {
	return !!identificatiesoort && validateIdentificatieNummer(identificatienummer, identificatiesoort).length === 0
}
