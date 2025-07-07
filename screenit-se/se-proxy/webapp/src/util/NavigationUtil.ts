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
import {Dispatch} from "redux"
import {
	createActionNavigateToClientgegevens,
	createActionNavigateToConnectiestatus,
	createActionNavigateToDaglijst,
	createActionNavigateToDagverslag,
	createActionNavigateToKwaliteitsopname,
	createActionNavigateToOnderzoek,
	NavigationActions,
} from "../actions/NavigationActions"
import {putNavigationState} from "../restclient/NavigationStateRestClient"
import {SubPagina} from "../datatypes/Navigation"

export const navigateToDaglijst = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToDaglijst())
}

export const navigateToClientgegevens = (dispatch: Dispatch, clientId: number, afspraakId: number): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToClientgegevens(clientId, afspraakId))
}

export const navigateToOnderzoek = (dispatch: Dispatch, clientId: number, afspraakId: number, subPagina: SubPagina): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToOnderzoek(clientId, afspraakId, subPagina))
}

export const navigateToDagverslag = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToDagverslag())
}

export const navigateToKwaliteitsopname = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToKwaliteitsopname())
}

export const navigateToConnectiestatus = (dispatch: Dispatch): void => {
	toProxyAndDispatch(dispatch, createActionNavigateToConnectiestatus())
}

export const restoreNavigation = (dispatch: Dispatch, navigatieActie: NavigationActions): void => {
	toProxyAndDispatch(dispatch, navigatieActie)
}

function toProxyAndDispatch(dispatchfunctie: Dispatch, action: NavigationActions): void {
	putNavigationState(action)
	dispatchfunctie(action)
}
