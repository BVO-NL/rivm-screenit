/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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
import TelefoonView, {TelefoonViewDispatchProps, TelefoonViewStateProps} from "./TelefoonView"
import {getMandatory} from "../../util/MapUtil"
import {dispatchActions} from "../../util/DispatchUtil"
import {createActionSetTelefoon1, createActionSetTelefoon2} from "../../actions/ClientActions"
import {RootState} from "../../Store"
import {Dispatch} from "redux"

export type TelefoonContainerProps = {
	telefoonnummer1?: string;
	telefoonnummer2?: string;
	disabled: boolean;
}

const mapStateToProps = (state: RootState, ownProps: TelefoonContainerProps): TelefoonViewStateProps => {
	const {clientId} = state.navigation
	return {
		...ownProps,
		clientId: clientId,
		clientGegevensForm: getMandatory(state.formsByFormId, "clientgegevens"),
	}
}

const mapDispatchToProps = (dispatch: Dispatch): TelefoonViewDispatchProps => ({
	setTelefoon1(clientId: number, telefoon: string): void {
		dispatchActions(dispatch, createActionSetTelefoon1(clientId, telefoon))
	},
	setTelefoon2(clientId: number, telefoon: string): void {
		dispatchActions(dispatch, createActionSetTelefoon2(clientId, telefoon))
	},
})

const TelefoonContainer = connect(mapStateToProps, mapDispatchToProps)(TelefoonView)
export default TelefoonContainer
