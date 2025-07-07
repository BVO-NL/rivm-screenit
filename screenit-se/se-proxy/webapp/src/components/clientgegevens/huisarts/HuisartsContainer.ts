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
import type {HuisartsViewDispatchProps, HuisartsViewStateProps} from "./HuisartsView"
import HuisartsView from "../huisarts/HuisartsView"
import {getIfExists, getMandatory} from "../../../util/MapUtil"
import {createActionKiesGeenHuisartsOptie, createActionKiesHuisarts} from "../../../actions/HuisartsActions"
import type {GeenHuisartsOption} from "../../../datatypes/Huisarts"
import {Huisarts} from "../../../datatypes/Huisarts"
import type {Afspraak} from "../../../datatypes/Afspraak"
import type {Client} from "../../../datatypes/Client"
import {dispatchActions} from "../../../util/DispatchUtil"
import {RootState} from "../../../Store"
import {Dispatch} from "redux"

export type HuisartsContainerProps = {
	afspraak: Afspraak;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: HuisartsContainerProps): HuisartsViewStateProps | undefined => {
	if (ownProps.afspraak) {
		const huisartsId = getMandatory(state.afsprakenById, ownProps.afspraak.id).huisartsId
		const huisarts = huisartsId ? getIfExists(state.huisartsenById, huisartsId) : undefined
		const client: Client = getMandatory(state.clientenById, ownProps.afspraak.clientId)
		const geenHuisartsOptie = ownProps.afspraak.geenHuisartsOptie
		const form = getMandatory(state.formsByFormId, "clientgegevens")
		return {
			afspraak: ownProps.afspraak,
			disabled: ownProps.disabled,
			clientPlaats: client.adres.plaats,
			huisarts: huisarts,
			geenHuisartsOptie: geenHuisartsOptie,
			clientGegevensForm: form,
			isValid: (form.isSubmitted && !!(huisarts || geenHuisartsOptie)) || !form.isSubmitted,
		}
	}
}

const mapDispatchToProps = (dispatch: Dispatch): HuisartsViewDispatchProps => ({
	onKiesHuisarts(afspraak: Afspraak, huisarts: Huisarts): void {
		dispatchActions(dispatch, createActionKiesHuisarts(afspraak.id, huisarts.id))
	},
	onKiesGeenHuisartsOptie(afspraak: Afspraak, geenHuisartsOptie: GeenHuisartsOption): void {
		dispatchActions(dispatch, createActionKiesGeenHuisartsOptie(afspraak.id, geenHuisartsOptie))
	},
})

const HuisartsContainer = connect(mapStateToProps, mapDispatchToProps)(HuisartsView)
export default HuisartsContainer
