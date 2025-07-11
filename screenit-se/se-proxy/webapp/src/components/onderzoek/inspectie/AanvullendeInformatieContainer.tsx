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
import {connect} from "react-redux"
import React from "react"
import AanvullendeInformatieView, {AanvullendeInformatieViewDispatchProps, AanvullendeInformatieViewStateProps, DUBBELE_TIJD_FIELD_ID} from "./AanvullendeInformatieView"
import type {OnvolledigOnderzoekOption} from "../../../datatypes/visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek"
import type {OnderbrokenOnderzoekOption} from "../../../datatypes/visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek"
import {
	createActionMaakAdviesHuisarts,
	createActionMaakDubbeleTijd,
	createActionMaakDubbeleTijdReden,
	createActionMaakEerderMammogramJaartal,
	createActionMaakEerderMammogramZorginstelling,
	createActionMaakExtraFotosRedenen,
	createActionMaakOnderbrokenOnderzoek,
	createActionMaakOnvolledigOnderzoek,
	createActionSetOnderzoekType,
} from "../../../actions/AanvullendeInformatieActions"
import type {ExtraFotosReden} from "../../../datatypes/visueleinspectie/aanvullendeinformatie/ExtraFotosReden"
import {getMandatory} from "../../../util/MapUtil"
import type {Form, FORM_FIELD_ID} from "../../../datatypes/Form"
import {validateField} from "../../../util/ValidationUtil"
import {createActionUpdateFormField} from "../../../actions/FormActions"
import {dispatchActions} from "../../../util/DispatchUtil"
import {RootState, store} from "../../../Store"
import moment from "moment"
import type {Zorginstelling} from "../../../datatypes/Zorginstelling"
import {createActionShowPopup} from "../../../actions/PopupActions"
import AdhocKwaliteitscontrolePopupView from "../../melding/AdhocKwaliteitscontrolePopupView"
import {magAdhocVersturen} from "../../../restclient/AdhocMeekijkverzoekRestClient"
import {showErrorToast} from "../../../util/ToastUtil"
import {Dispatch} from "redux"
import {OnderzoekType} from "../../../datatypes/OnderzoekType"

const aantalJarenTerug = 4
type AanvullendeInformatieContainerProps = {
	disabled: boolean;
	voegOnderzoekTypeWijzigingToeAanWerklijst?: (onderzoekType: OnderzoekType) => void;
};

const mapStateToProps = (state: RootState, ownProps: AanvullendeInformatieContainerProps): AanvullendeInformatieViewStateProps => {
	const disabled = ownProps.disabled
	const onderzoek = getMandatory(state.onderzoekByAfspraakId, state.navigation.afspraakId)
	const afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)
	const client = getMandatory(state.clientenById, state.navigation.clientId)
	return {
		online: state.online,
		jaren: getJarenLijst(state.daglijstDatum),
		zorginstellingen: Array.from(store.getState().zorginstellingen.values()),
		eerderMammogramZorginstelling: onderzoek ? onderzoek.eerderMammogramZorginstellingId ? state.zorginstellingen.get(onderzoek.eerderMammogramZorginstellingId) : undefined : undefined,
		eerderMammogramJaartal: onderzoek ? onderzoek.eerderMammogramJaartal : undefined,
		onvolledigOnderzoek: onderzoek.onvolledigOnderzoek,
		onderbrokenOnderzoek: onderzoek.onderbrokenOnderzoek,
		extraFotosRedenen: onderzoek.extraFotosRedenen,
		onderzoekType: onderzoek.onderzoekType,
		dubbeleTijd: client.doelgroep === "DUBBELE_TIJD",
		dubbeleTijdReden: client.dubbeleTijdReden,
		adviesHuisarts: onderzoek.adviesHuisarts,
		afspraakId: afspraak.id,
		clientId: client.id,
		disabled: afspraak.doorgevoerd || disabled,
		onderzoekForm: getMandatory(state.formsByFormId, "onderzoek"),
		dubbeleTijdDisabled: client.doelgroep === "MINDER_VALIDE",
		tomosyntheseMogelijk: true === state.environmentInfo?.tomosyntheseMogelijk,
	}
}

const getJarenLijst = (daglijstDatum: string): number[] => {
	const ditJaar: number = moment(daglijstDatum).year()
	const jarenList: number[] = []
	for (let i = 0; i < aantalJarenTerug; i++) {
		jarenList.push(ditJaar - i)
	}
	return jarenList
}

const mapDispatchToProps = (dispatch: Dispatch, ownProps: AanvullendeInformatieContainerProps): AanvullendeInformatieViewDispatchProps => {
	return {
		verwerkEerderMammogramZorginstelling: (afspraakId: number, eerderMammogramZorginstelling: Zorginstelling | undefined): void => {
			dispatchActions(dispatch, createActionMaakEerderMammogramZorginstelling(afspraakId, eerderMammogramZorginstelling ? eerderMammogramZorginstelling.id : undefined))
		},
		verwerkEerderMammogramJaartal: (afspraakId: number, jaartal: number | undefined): void => {
			dispatchActions(dispatch, createActionMaakEerderMammogramJaartal(afspraakId, jaartal))
		},
		verwerkOnvolledigOnderzoek(afspraakId: number, onvolledigOnderzoek: OnvolledigOnderzoekOption | undefined): void {
			if (onvolledigOnderzoek === "ZONDER_FOTOS") {
				dispatchActions(dispatch, createActionMaakExtraFotosRedenen(afspraakId, undefined))
			}
			dispatchActions(dispatch, createActionMaakOnvolledigOnderzoek(afspraakId, onvolledigOnderzoek))
		},
		verwerkOnderbrokenOnderzoek(afspraakId: number, onderbrokenOnderzoek: OnderbrokenOnderzoekOption | undefined): void {
			dispatchActions(dispatch, createActionMaakOnderbrokenOnderzoek(afspraakId, onderbrokenOnderzoek))
		},
		verwerkExtraFotosReden(afspraakId: number, extraFotosRedenen: Array<ExtraFotosReden>): void {
			dispatchActions(dispatch, createActionMaakExtraFotosRedenen(afspraakId, extraFotosRedenen))
		},
		verwerkOnderzoekType(afspraakId: number, onderzoekType: OnderzoekType): void {
			dispatchActions(dispatch, createActionSetOnderzoekType(afspraakId, onderzoekType))
		},
		verwerkDubbeleTijd(afspraakId: number, clientId: number, dubbeleTijd: boolean, dubbeleTijdReden: string | undefined, form: Form): void {
			dispatchActions(dispatch, createActionMaakDubbeleTijd(afspraakId, clientId, dubbeleTijd))
			this.updateField({
				dubbeleTijd: dubbeleTijd,
				dubbeleTijdReden: dubbeleTijdReden,
			}, DUBBELE_TIJD_FIELD_ID, form, false)
		},
		verwerkDubbeleTijdReden(afspraakId: number, clientId: number, dubbeleTijdReden: string): void {
			dispatchActions(dispatch, createActionMaakDubbeleTijdReden(afspraakId, clientId, dubbeleTijdReden))
		},
		verwerkAdviesHuisarts(afspraakId: number, adviesHuisarts: string): void {
			dispatchActions(dispatch, createActionMaakAdviesHuisarts(afspraakId, adviesHuisarts))
		},
		updateField<T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined): void {
			const field = validateField(value, fieldId, form, showError)
			dispatchActions(dispatch, createActionUpdateFormField(form.formId, fieldId, field))
		},
		adhocKwaliteitscontrolePopup(afspraakId: number): void {
			magAdhocVersturen(afspraakId).then(responseCode => {
				if (responseCode === 200) {
					dispatch(createActionShowPopup("Meekijkverzoek LRCB", <AdhocKwaliteitscontrolePopupView
						afspraakId={afspraakId}/>, () => {
					}))
				} else if (responseCode === 412) {
					showErrorToast("Voor dit onderzoek is al een meekijkverzoek LRCB aangevraagd")
				} else if (responseCode === 409) {
					showErrorToast("Dit onderzoek is doorgevoerd, hiervoor kan geen meekijkverzoek meer worden aangevraagd.")
				} else {
					showErrorToast("Kan geen meekijkverzoek LRCB aanvragen, probeer het later opnieuw")
				}
			})
		},
		voegOnderzoekTypeWijzigingToeAanWerklijst(onderzoekType: OnderzoekType): void {
			if (ownProps.voegOnderzoekTypeWijzigingToeAanWerklijst) {
				ownProps.voegOnderzoekTypeWijzigingToeAanWerklijst(onderzoekType)
			}
		},
	}
}

const AanvullendeInformatieContainer = connect(mapStateToProps, mapDispatchToProps)(AanvullendeInformatieView)
export default AanvullendeInformatieContainer
