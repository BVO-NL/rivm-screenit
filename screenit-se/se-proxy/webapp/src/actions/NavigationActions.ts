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
import type {SubPagina} from "../datatypes/Navigation"

export type NavigationActions =
	| NavigateToDaglijstAction
	| NavigateToClientgegevensAction
	| NavigateToOnderzoekAction
	| NavigateToDagverslagAction
	| NavigateToKwaliteitsopnameAction
	| NavigateToConnectiestatusAction
	| ClearNavigationAction;

export const NAVIGATE_TO_DAGLIJST = "NAVIGATE_TO_DAGLIJST"
export type NavigateToDaglijstAction = {
	type: "NAVIGATE_TO_DAGLIJST";
};
export const createActionNavigateToDaglijst = (): NavigateToDaglijstAction => {
	const action: NavigateToDaglijstAction = {
		type: NAVIGATE_TO_DAGLIJST,
	}
	return action
}

export const NAVIGATE_TO_CLIENTGEGEVENS = "NAVIGATE_TO_CLIENTGEGEVENS"
export type NavigateToClientgegevensAction = {
	type: "NAVIGATE_TO_CLIENTGEGEVENS";
	clientId: number;
	afspraakId: number;
};
export const createActionNavigateToClientgegevens = (clientId: number, afspraakId: number): NavigateToClientgegevensAction => {
	const action: NavigateToClientgegevensAction = {
		type: NAVIGATE_TO_CLIENTGEGEVENS,
		clientId: clientId,
		afspraakId: afspraakId,
	}
	return action
}

export const NAVIGATE_TO_ONDERZOEK = "NAVIGATE_TO_ONDERZOEK"
export type NavigateToOnderzoekAction = {
	type: "NAVIGATE_TO_ONDERZOEK";
	clientId: number;
	afspraakId: number;
	subPagina: SubPagina;
};
export const createActionNavigateToOnderzoek = (clientId: number, afspraakId: number, subPagina: SubPagina): NavigateToOnderzoekAction => {
	const action: NavigateToOnderzoekAction = {
		type: NAVIGATE_TO_ONDERZOEK,
		clientId: clientId,
		afspraakId: afspraakId,
		subPagina: subPagina,
	}
	return action
}

export const NAVIGATE_TO_DAGVERSLAG = "NAVIGATE_TO_DAGVERSLAG"
export type NavigateToDagverslagAction = {
	type: "NAVIGATE_TO_DAGVERSLAG";
	clientId?: number;
	afspraakId?: number;
	subPagina?: SubPagina;
};
export const createActionNavigateToDagverslag = (): NavigateToDagverslagAction => {
	const action: NavigateToDagverslagAction = {
		type: NAVIGATE_TO_DAGVERSLAG,
		clientId: undefined,
		afspraakId: undefined,
		subPagina: undefined,
	}
	return action
}

export type RestoredNavigationAction = {
	type: string;
	clientId: number;
	afspraakId: number;
	subPagina: SubPagina;
}

export const NAVIGATE_TO_KWALITEITSOPNAME = "NAVIGATE_TO_KWALITEITSOPNAME"
export type NavigateToKwaliteitsopnameAction = {
	type: "NAVIGATE_TO_KWALITEITSOPNAME";
};
export const createActionNavigateToKwaliteitsopname = (): NavigateToKwaliteitsopnameAction => {
	const action: NavigateToKwaliteitsopnameAction = {
		type: NAVIGATE_TO_KWALITEITSOPNAME,
	}
	return action
}

export const NAVIGATE_TO_CONNECTIESTATUS = "NAVIGATE_TO_CONNECTIESTATUS"
export type NavigateToConnectiestatusAction = {
	type: "NAVIGATE_TO_CONNECTIESTATUS";
};
export const createActionNavigateToConnectiestatus = (): NavigateToConnectiestatusAction => {
	const action: NavigateToConnectiestatusAction = {
		type: NAVIGATE_TO_CONNECTIESTATUS,
	}
	return action
}

export const CLEAR_NAVIGATION = "CLEAR_NAVIGATION"
export type ClearNavigationAction = {
	type: "CLEAR_NAVIGATION";
};
export const createActionClearNavigation = (): ClearNavigationAction => ({
	type: CLEAR_NAVIGATION,
})
