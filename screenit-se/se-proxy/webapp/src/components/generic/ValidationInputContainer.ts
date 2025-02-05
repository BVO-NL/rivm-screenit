/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {connect} from "react-redux"
import ValidationInputValue, {ValidationInputValueDispatchProps, ValidationInputValueStateProps} from "./ValidationInputValue"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import {createActionUpdateFormField} from "../../actions/FormActions"
import {validateField} from "../../util/ValidationUtil"
import {getMandatory} from "../../util/MapUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"
import {Validation} from "../../validation/Validation"
import {InputType} from "reactstrap/es/Input"

export type ValidationInputContainerProps<T> = {
	id?: string;
	value: string;
	label?: string;
	transformValue?: (arg0: string) => T;
	fieldId: FORM_FIELD_ID;
	maxLength?: number;
	disabled: boolean;
	placeholder?: string;
	type?: InputType;
	color?: string;
	className?: string;
	validator?: Validation<T>;
	showRedErrorBackground?: boolean;
	onChange?: (inputValue: string) => void;
};

const mapStateToProps = (state: RootState, ownProps: ValidationInputContainerProps<any>): ValidationInputValueStateProps<any> => {
	return {
		...ownProps,
		form: getMandatory(state.formsByFormId, ownProps.fieldId.formId),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): ValidationInputValueDispatchProps<any> => ({
	updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): void {
		const field = validateField(value, fieldId, form, showError)
		dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field))
	},
})

const ValidationInputContainer = connect(mapStateToProps, mapDispatchToProps)(ValidationInputValue)
export default ValidationInputContainer
