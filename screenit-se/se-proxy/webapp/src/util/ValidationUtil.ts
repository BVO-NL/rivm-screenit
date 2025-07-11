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
import type {Form, FORM_FIELD_ID, FormField} from "../datatypes/Form"
import {getMandatory} from "./MapUtil"

export const isFormValid = (form: Form): boolean => {
	let isValid = true
	for (const field of form.fieldsById.values()) {
		if (!field.validator.isValid(field.value)) {
			isValid = false
			break
		}
	}
	return isValid
}

export const submitForm = (form: Form): Form => {
	const formFields: Map<FORM_FIELD_ID, FormField<any>> = new Map()

	for (const key of form.fieldsById.keys()) {
		formFields.set(key, {
			...getMandatory(form.fieldsById, key),
			...{
				showError: true,
			},
		})
		let field: FormField<any> = getMandatory(formFields, key)
		if (!field.isValid && field.errorMessage === "") {
			field = validate(field, field.value)
		}
	}
	return {
		formId: form.formId,
		fieldsById: formFields,
		isSubmitted: true,
	}
}

const validate = (formField: FormField<any>, value: any): FormField<any> => {
	formField.isValid = formField.validator.isValid(value)
	formField.errorMessage = formField.validator.getErrorMessage(value, formField.label)
	return formField
}

export const validateField = (value: any, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): FormField<any> => {
	let formField: FormField<any> = {
		...getMandatory(form.fieldsById, fieldId),
	}
	formField.value = value
	formField = validate(formField, value)

	if (showError === true || showError === false) {
		formField.showError = showError
	}

	return formField
}
