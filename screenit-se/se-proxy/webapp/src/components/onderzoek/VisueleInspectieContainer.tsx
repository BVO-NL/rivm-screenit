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
import VisueleInspectieView, {VisueleInspectieViewDispatchProps, VisueleInspectieViewStateProps} from "./VisueleInspectieView"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {ClientWerklijstItem} from "../../datatypes/ClientWerklijstItem"
import {leesAfspraken} from "../../restclient/DaglijstRestclient"
import {getIfExists, getMandatory} from "../../util/MapUtil"
import type {Onderzoek} from "../../datatypes/Onderzoek"
import type {AnnotatieAfbeelding} from "../../datatypes/AnnotatieAfbeelding"
import {mapAfbeeldingToEnkelDto} from "../../datatypes/AnnotatieAfbeelding"
import {
	createActionDeleteVisueleInspectieAfbeeldingByAfspraakId,
	createActionMammografieOpslaan,
	createActionMammografieWijzigen,
	createActionSetVisueleInspectieAfbeelding,
} from "../../actions/VisueleInspectieActions"
import {createActionAfspraakSignaleren} from "../../actions/AfspraakActions"
import type {Form, FORM_FIELD_ID, FormField} from "../../datatypes/Form"
import {initialFormField} from "../../datatypes/Form"
import {DUBBELE_TIJD_FIELD_ID} from "./inspectie/AanvullendeInformatieView"
import {DubbeleTijdRedenValidator} from "../../validation/DubbeleTijdRedenValidator"
import {isFormValid, submitForm} from "../../util/ValidationUtil"
import {showErrorToast, showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import {createActionUpdateForm} from "../../actions/FormActions"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {dispatchActions} from "../../util/DispatchUtil"
import type {Client} from "../../datatypes/Client"
import {createDubbeleTijdActions} from "../../util/OnderzoekUtil"
import {createActionWijzigingenVerwerkt} from "../../actions/WijzigingenActions"
import {RootState, store} from "../../Store"
import {checkOpBeeldenVanAmputatie, clientToevoegenAanWerklijst, verwijderVanWerklijst, waarschuwingGecontroleerd} from "../../restclient/WerklijstRestclient"
import {showAmputatieWaarschuwingPopup, showWijzigingenPopup} from "../../util/PopupUtil"
import {createActionOnderzoekOpslaan} from "../../actions/OnderzoekActions"
import {createActionClearPopup} from "../../actions/PopupActions"
import {Dispatch} from "redux"
import {SeAction} from "../../actions/SeAction"
import {navigateToDaglijst, navigateToOnderzoek} from "../../util/NavigationUtil"

export type VisueleInspectieContainerProps = {
	client: Client;
	afspraak: Afspraak;
};

const mapStateToProps = (state: RootState, ownProps: VisueleInspectieContainerProps): VisueleInspectieViewStateProps => {
	const aanvullendeInformatieForm = getIfExists(state.formsByFormId, "onderzoek")
	const onderzoek = getMandatory(state.onderzoekByAfspraakId, state.navigation.afspraakId)
	const afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)
	const isEditable = !afspraak ? false : !afspraak.doorgevoerd
	return {
		...ownProps,
		magSignaleren: state.autorisatie.signaleren,
		onderzoek: onderzoek,
		heeftWijzigingen: state.heeftWijzigingen,
		afbeelding: getIfExists(state.visueleInspectieAfbeeldingByAfspraakId, state.navigation.afspraakId) || {
			afspraakId: state.navigation.afspraakId,
			iconenById: new Map(),
		} as AnnotatieAfbeelding,
		aanvullendeInformatieForm: aanvullendeInformatieForm ? aanvullendeInformatieForm : newAanvullendeInformatieForm(getMandatory(state.clientenById, state.navigation.clientId)),
		isEditable: isEditable,
		aeTitle: state.huidigeMammograafId ? getMandatory(state.mammografenById, state.huidigeMammograafId).aeTitle : undefined,
		medewerkercode: state.session?.medewerkercode,
	}
}

export const newAanvullendeInformatieForm = (client: Client): Form => {
	const fieldsMap: Map<FORM_FIELD_ID, FormField<any>> = new Map()
	fieldsMap.set(DUBBELE_TIJD_FIELD_ID, initialFormField({
		dubbeleTijd: client.doelgroep === "DUBBELE_TIJD",
		dubbeleTijdReden: client.dubbeleTijdReden,
	}, "Reden dubbele tijd", new DubbeleTijdRedenValidator()))
	return {
		formId: "onderzoek",
		fieldsById: fieldsMap,
		isSubmitted: false,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): VisueleInspectieViewDispatchProps => ({
	onVorige(afspraak: Afspraak): void {
		if (store.getState().heeftWijzigingen) {
			showWijzigingenPopup(() => {
				leesAfspraken(store.getState().daglijstDatum, undefined, false)
				dispatch(createActionDeleteVisueleInspectieAfbeeldingByAfspraakId(afspraak.id))
				dispatch(createActionWijzigingenVerwerkt())
				navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Vorige onderzoeken")
			}, dispatch)
		} else {
			navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Vorige onderzoeken")
		}
	},
	onVolgende(afspraak: Afspraak, client: Client, onderzoek: Onderzoek, afbeelding: AnnotatieAfbeelding, magSignaleren: boolean, form: Form, alleenOpslaan: boolean): void {
		if (!onderzoek.onvolledigOnderzoek && !onderzoek.onderbrokenOnderzoek) {
			onderzoek.status = "AFGEROND"
		}

		checkOpBeeldenVanAmputatie(String(afspraak.uitnodigingsNr), onderzoek.amputatie).then((amputatieConflict) => {
			if (amputatieConflict) {
				showAmputatieWaarschuwingPopup(() => {
					waarschuwingGecontroleerd(afspraak.uitnodigingsNr, client.id)
					dispatchActions(dispatch, createActionClearPopup())
					opslaanEnNavigeren(dispatch, afspraak, client, onderzoek, afbeelding, magSignaleren, form, alleenOpslaan)
				}, dispatch)
			} else {
				opslaanEnNavigeren(dispatch, afspraak, client, onderzoek, afbeelding, magSignaleren, form, alleenOpslaan)
			}
		})
	},
	toevoegenAanWerklijst(werklijstItem: ClientWerklijstItem): void {
		clientToevoegenAanWerklijst(werklijstItem)
	},
	verwijderVanWerklijst(aeTitle: string): void {
		verwijderVanWerklijst(aeTitle)
	},
	onInitializeForm(client: Client): void {
		dispatch(createActionUpdateForm("onderzoek", newAanvullendeInformatieForm(client)))
	},
})

const opslaanEnNavigeren = (dispatch: Dispatch, afspraak: Afspraak, client: Client, onderzoek: Onderzoek, afbeelding: AnnotatieAfbeelding, magSignaleren: boolean, form: Form, alleenOpslaan: boolean): void => {
	if (!alleenOpslaan && afspraak.status !== "ONDERZOEK" && store.getState().heeftWijzigingen) {
		showWijzigingenPopup(() => {
			leesAfspraken(store.getState().daglijstDatum, undefined, false)
			dispatch(createActionDeleteVisueleInspectieAfbeeldingByAfspraakId(afspraak.id))
			dispatch(createActionWijzigingenVerwerkt())
			navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Signaleren")
		}, dispatch)
	} else if (!alleenOpslaan && afspraak.status !== "ONDERZOEK") {
		navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Signaleren")
	} else {
		dispatchActions(dispatch, createActionUpdateForm("onderzoek", submitForm(form)))
		if (!isFormValid(form)) {
			showErrorToast("De ingevoerde gegevens zijn niet valide.")
			return
		}
		const onderzoekActions = maakOnderzoekActions(afspraak, afbeelding, onderzoek, client, alleenOpslaan)
		putTransactionToScreenItCentraalPromise(afspraak, "VISUELE_INSPECTIE_OPSLAAN", ...onderzoekActions).then(() => {
			showWijzigingenOpgeslagenToast()
		})
		dispatchActions(dispatch, createActionWijzigingenVerwerkt())
		if (!alleenOpslaan) {
			dispatch(createActionAfspraakSignaleren(afspraak.id))
			if (magSignaleren) {
				navigateToOnderzoek(dispatch, afspraak.clientId, afspraak.id, "Signaleren")
			} else {
				navigateToDaglijst(dispatch)
			}
		}
	}
}

const maakOnderzoekActions = (afspraak: Afspraak, afbeelding: AnnotatieAfbeelding, onderzoek: Onderzoek, client: Client, alleenOpslaan: boolean): SeAction[] => {
	const onderzoekActions: SeAction[] = []
	onderzoekActions.push(createActionOnderzoekOpslaan(afspraak.id, onderzoek))
	onderzoekActions.push(createActionSetVisueleInspectieAfbeelding(afspraak.id, mapAfbeeldingToEnkelDto(afspraak.id, afbeelding)))
	onderzoekActions.push(...createDubbeleTijdActions(afspraak, client))
	if (afspraak.status === "ONDERZOEK" && !alleenOpslaan) {
		onderzoekActions.push(createActionMammografieOpslaan(afspraak.id, afbeelding))
		onderzoekActions.push(createActionAfspraakSignaleren(afspraak.id))
	} else {
		onderzoekActions.push(createActionMammografieWijzigen(afspraak.id, afbeelding))
	}
	return onderzoekActions
}

const VisueleInspectieContainer = connect(mapStateToProps, mapDispatchToProps)(VisueleInspectieView)
export default VisueleInspectieContainer
