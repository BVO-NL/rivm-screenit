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
import MbbSignaleringView, {MbbSignaleringViewDispatchProps, MbbSignaleringViewStateProps} from "./MbbSignaleringView"
import {getIfExists, getMandatory} from "../../../util/MapUtil"
import {
	createActionMaakAanvullendeInformatieOperatie,
	createActionMaakExtraMedewerker,
	createActionMaakMbbOpmerking,
	createActionMaakRadioloogOpmerking,
	createActionMaakRedenFotobespreking,
	createActionMaakSuboptimaleInsteltechniek,
	createActionOperatieLinks,
	createActionOperatieRechts,
} from "../../../actions/MBBSignaleringActions"
import {dispatchActions} from "../../../util/DispatchUtil"
import {RootState, store} from "../../../Store"
import type {SuboptimaleInsteltechniek} from "../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek"
import type {RedenFotobespreking} from "../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking"
import {Dispatch} from "redux"

export type MbbSignalerenContainerProps = {
	disabled: boolean;
};

const mapStateToProps = (state: RootState, ownProps: MbbSignalerenContainerProps): MbbSignaleringViewStateProps => {
	const disabled = ownProps.disabled
	const onderzoek = getIfExists(state.onderzoekByAfspraakId, state.navigation.afspraakId)
	const afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)
	const seGebruikers = store.getState().seGebruikers
	return {
		suboptimaleInsteltechniek: onderzoek?.suboptimaleInsteltechniek,
		redenFotobespreking: onderzoek?.redenFotobespreking,
		extraMedewerkerId: onderzoek?.extraMedewerkerId,
		seGebruikers: seGebruikers,
		ingelogdeGebruikerId: store.getState().session?.instellingGebruikerId,
		opmerkingMbber: onderzoek?.opmerkingMbber,
		opmerkingVoorRadioloog: onderzoek?.opmerkingVoorRadioloog,
		operatieRechts: onderzoek?.operatieRechts,
		operatieLinks: onderzoek?.operatieLinks,
		aanvullendeInformatieOperatie: onderzoek?.aanvullendeInformatieOperatie,
		afspraakId: afspraak.id,
		disabled: (afspraak.doorgevoerd || disabled),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): MbbSignaleringViewDispatchProps => {
	return {
		verwerkInsteltechniek: (afspraakId: number, suboptimaleInsteltechniek?: SuboptimaleInsteltechniek): void => {
			dispatchActions(dispatch, createActionMaakSuboptimaleInsteltechniek(afspraakId, suboptimaleInsteltechniek))
		},
		verwerkRedenFotobespreking: (afspraakId: number, redenFotobespreking?: RedenFotobespreking): void => {
			dispatchActions(dispatch, createActionMaakRedenFotobespreking(afspraakId, redenFotobespreking))
		},
		verwerkExtraMedewerkerId: (afspraakId: number, extraMedewerkerId?: number): void => {
			dispatchActions(dispatch, createActionMaakExtraMedewerker(afspraakId, extraMedewerkerId))
		},
		verwerkOpmerkingMbber: (afspraakId: number, opmerkingMbber: string): void => {
			dispatchActions(dispatch, createActionMaakMbbOpmerking(afspraakId, opmerkingMbber))
		},
		verwerkOpmerkingVoorRadioloog: (afspraakId: number, opmerkingVoorRadioloog: string): void => {
			dispatchActions(dispatch, createActionMaakRadioloogOpmerking(afspraakId, opmerkingVoorRadioloog))
		},
		verwerkOperatieRechtsChanged: (afspraakId: number, operatieRechts: boolean): void => {
			dispatchActions(dispatch, createActionOperatieRechts(afspraakId, operatieRechts))
		},
		verwerkOperatieLinksChanged: (afspraakId: number, operatieLinks: boolean): void => {
			dispatchActions(dispatch, createActionOperatieLinks(afspraakId, operatieLinks))
		},
		verwerkaanvullendeInformatieOperatie: (afspraakId: number, aanvullendeInformatieOperatie: string): void => {
			dispatchActions(dispatch, createActionMaakAanvullendeInformatieOperatie(afspraakId, aanvullendeInformatieOperatie))
		},
	}
}

const MbbSignaleringContainer = connect(mapStateToProps, mapDispatchToProps)(MbbSignaleringView)
export default MbbSignaleringContainer
