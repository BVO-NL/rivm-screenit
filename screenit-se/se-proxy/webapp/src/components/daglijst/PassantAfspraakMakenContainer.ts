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
import PassantAfspraakMakenView, {
	BSN_FIELD_ID,
	GEBOORTEDATUM_FIELD_ID,
	PassantAfspraakMakenViewDispatchProps,
	PassantAfspraakMakenViewStateProps,
} from "./PassantAfspraakMakenView"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import type {Form, FORM_FIELD_ID, FormField} from "../../datatypes/Form"
import {initialFormField} from "../../datatypes/Form"
import {GeboortedatumValidator} from "../../validation/GeboortedatumValidator"
import {BsnValidator} from "../../validation/BSNValidator"
import {createActionUpdateForm} from "../../actions/FormActions"
import {dispatchActions} from "../../util/DispatchUtil"
import {isFormValid, submitForm} from "../../util/ValidationUtil"
import {showErrorToast} from "../../util/ToastUtil"
import {readPassant} from "../../restclient/PassantZoekenRestClient"
import {datumInToekomst, datumInVerleden} from "../../util/DateUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): PassantAfspraakMakenViewStateProps => {
	const passantAfspraakMakenForm = getIfExists(state.formsByFormId, "passant_afspraak_maken")
	return {
		heeftInschrijvenRecht: state.autorisatie.inschrijven,
		online: state.online,
		datumNietVandaag: datumInVerleden(state.daglijstDatum) || datumInToekomst(state.daglijstDatum),
		passantAfspraakMakenForm: passantAfspraakMakenForm ? passantAfspraakMakenForm : newPassantAfspraakMakenForm(),
	}
}

export const newPassantAfspraakMakenForm = (): Form => {
	const fieldsMap: Map<FORM_FIELD_ID, FormField<any>> = new Map()
	fieldsMap.set(GEBOORTEDATUM_FIELD_ID, initialFormField("", "Geboortedatum", new GeboortedatumValidator()))
	fieldsMap.set(BSN_FIELD_ID, initialFormField("", "BSN", new BsnValidator()))
	return {
		formId: "passant_afspraak_maken",
		fieldsById: fieldsMap,
		isSubmitted: false,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): PassantAfspraakMakenViewDispatchProps => ({
	onInitializeForm(): void {
		dispatch(createActionUpdateForm("passant_afspraak_maken", newPassantAfspraakMakenForm()))
	},
	maakAfspraak(form: Form): void {
		dispatchActions(dispatch, createActionUpdateForm("passant_afspraak_maken", submitForm(form)))

		if (!isFormValid(form)) {
			showErrorToast("De ingevoerde cliëntgegevens zijn niet valide.")
			return
		}

		const geboortedatum: string = getMandatory(form.fieldsById, GEBOORTEDATUM_FIELD_ID).value
		const bsn: string = getMandatory(form.fieldsById, BSN_FIELD_ID).value
		readPassant(bsn, geboortedatum, dispatch)
	},
})

const PassantAfspraakMakenContainer = connect(mapStateToProps, mapDispatchToProps)(PassantAfspraakMakenView)
export default PassantAfspraakMakenContainer
