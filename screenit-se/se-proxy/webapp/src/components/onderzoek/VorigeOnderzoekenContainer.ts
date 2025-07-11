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
import type {VorigeOnderzoekenConfirmBtnKind, VorigeOnderzoekenViewDispatchProps, VorigeOnderzoekenViewStateProps} from "./VorigeOnderzoekenView"
import VorigeOnderzoekenView from "./VorigeOnderzoekenView"
import {createActionOnderzoekStarten} from "../../actions/OnderzoekActions"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import {putTransactionToScreenItCentraalPromise} from "../../restclient/TransactionRestclient"
import {dispatchActions} from "../../util/DispatchUtil"
import {disablePrimarySeKnop} from "../../util/Util"
import {showWijzigingenOpgeslagenToast} from "../../util/ToastUtil"
import type {SeAction} from "../../actions/SeAction"
import {RootState} from "../../Store"
import {Dispatch} from "redux"
import {navigateToOnderzoek} from "../../util/NavigationUtil"
import {OnderzoekType} from "../../datatypes/OnderzoekType"

export const vorigeOnderzoekenVervolgBtnKind = (afspraak: Afspraak, tomosyntheseMogelijk: boolean): VorigeOnderzoekenConfirmBtnKind => {
	if (afspraak.status === "INGESCHREVEN") {
		return tomosyntheseMogelijk ? "Mammografie starten" : "Onderzoek starten"
	}
	return "Volgende"
}

export type VorigeOnderzoekenContainerProps = {
	afspraak: Afspraak;
	client: Client;
	gebruikersnaam?: string;
	setHeeftOudeBeeldenOpgevraagd: () => void;
};

const mapStateToProps = (state: RootState, ownProps: VorigeOnderzoekenContainerProps): VorigeOnderzoekenViewStateProps => {
	const tomosyntheseMogelijk: boolean = true === state.environmentInfo?.tomosyntheseMogelijk
	return {
		...ownProps,
		magOnderzoeken: state.autorisatie.onderzoeken,
		tomosyntheseMogelijk: tomosyntheseMogelijk,
		kanTomosyntheseStarten: tomosyntheseMogelijk && "INGESCHREVEN" === ownProps.afspraak.status,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): VorigeOnderzoekenViewDispatchProps => {
	return {
		onVervolgButton(client: Client, afspraak: Afspraak, onderzoekType: OnderzoekType, confirmBtnKind: VorigeOnderzoekenConfirmBtnKind): void {
			switch (confirmBtnKind) {
				case "Mammografie starten":
				case "Onderzoek starten":
					disablePrimarySeKnop()
					const actions: Array<SeAction> = []
					const amputatie = client.vorigeOnderzoeken && client.vorigeOnderzoeken[0] && client.vorigeOnderzoeken[0].onderzoek?.amputatie
					const onderzoekStartenAction = createActionOnderzoekStarten(afspraak.id, amputatie, onderzoekType)
					actions.push(onderzoekStartenAction)
					dispatchActions(dispatch, ...actions)
					navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
					putTransactionToScreenItCentraalPromise(afspraak, "ONDERZOEK_STARTEN", ...actions).then(() => {
						showWijzigingenOpgeslagenToast()
					})
					break
				case "Volgende":
					disablePrimarySeKnop()
					navigateToOnderzoek(dispatch, client.id, afspraak.id, "Visuele inspectie")
					break
				default:
					break
			}
		},

	}
}

const VorigeOnderzoekenContainer = connect(mapStateToProps, mapDispatchToProps)(VorigeOnderzoekenView)
export default VorigeOnderzoekenContainer
