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
import IdentificatieView, {IDENTIFICATIE_FIELD_ID, IdentificatieViewDispatchProps, IdentificatieViewStateProps} from "./IdentificatieView"
import type {Identificatiesoort} from "../../datatypes/Afspraak"
import {Afspraak} from "../../datatypes/Afspraak"
import {createActionKiesIdentificatienummer, createActionKiesIdentificatiesoort} from "../../actions/AfspraakActions"
import {getMandatory} from "../../util/MapUtil"
import {createActionUpdateFormField} from "../../actions/FormActions"
import type {Form, FORM_FIELD_ID, FormField} from "../../datatypes/Form"
import {validateField} from "../../util/ValidationUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {RootState} from "../../Store"
import {Dispatch} from "redux"

export type IdentificatieContainerProps = {
	afspraak: Afspraak;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: IdentificatieContainerProps): IdentificatieViewStateProps => {
	return {
		...ownProps,
		clientGegevensForm: getMandatory(state.formsByFormId, "clientgegevens"),
	}
}

const mapDispatchToProps = (dispatch: Dispatch, ownProps: IdentificatieContainerProps): IdentificatieViewDispatchProps => ({
	onChooseSoort(identificatiesoort: Identificatiesoort, form: Form): void {
		this.updateField({
			identificatiesoort: identificatiesoort,
			identificatienummer: undefined,
		}, IDENTIFICATIE_FIELD_ID, form, false)
		dispatchActions(dispatch, createActionKiesIdentificatiesoort(ownProps.afspraak.id, identificatiesoort))
		dispatchActions(dispatch, createActionKiesIdentificatienummer(ownProps.afspraak.id, undefined))
	},
	onChooseNummer(identificatienummer: string | undefined): void {
		dispatchActions(dispatch, createActionKiesIdentificatienummer(ownProps.afspraak.id, identificatienummer))
	},
	updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): void {
		const field: FormField<string | null | undefined> = validateField(value, fieldId, form, showError)
		dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field))
	},
})

const IdentificatieContainer = connect(mapStateToProps, mapDispatchToProps)(IdentificatieView)
export default IdentificatieContainer
