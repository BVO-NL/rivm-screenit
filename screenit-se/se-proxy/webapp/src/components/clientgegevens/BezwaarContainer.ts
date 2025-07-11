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
import type {BezwaarDispatchProps, BezwaarStateProps} from "./BezwaarView"
import BezwaarView from "./BezwaarView"
import {createActionBezwaarAanvragen} from "../../actions/AfspraakActions"
import {dispatchActions} from "../../util/DispatchUtil"
import {Dispatch} from "redux"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState, parentProps: BezwaarStateProps): BezwaarStateProps => {
	return {
		...parentProps,
		bezwaarDoorgevoerdOpCentraal: parentProps.isIngeschreven || parentProps.bezwaarDoorgevoerdOpCentraal,
	}
}

const mapDispatchToProps = (dispatch: Dispatch): BezwaarDispatchProps => ({
	onBezwaarAangevraagd(afspraakId: number, bezwaarAangevraagd: boolean, isIngeschreven: boolean): void {
		if (!isIngeschreven) {
			dispatchActions(dispatch, createActionBezwaarAanvragen(afspraakId, bezwaarAangevraagd))
		}
	},

})

const BezwaarContainer = connect(mapStateToProps, mapDispatchToProps)(BezwaarView)
export default BezwaarContainer
