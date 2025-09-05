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
import type {SeMedewerkersActions} from "../actions/SeMedewerkersActions"
import {ADD_ALL_SE_MEDEWERKERS, ADD_SE_MEDEWERKER, CLEAR_SE_MEDEWERKERS} from "../actions/SeMedewerkersActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const SeMedewerkersReducer: Reducer<Map<string, string>, SeMedewerkersActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	let result = new Map()
	if (!stateSlice) {
		stateSlice = result
	}
	switch (action.type) {
		case ADD_ALL_SE_MEDEWERKERS:
			result = new Map([...stateSlice, ...Object.entries(action.seMedewerkers)])
			break
		case ADD_SE_MEDEWERKER:
			result = stateSlice
			result.set(String(action.organisatieMedewerkerId), action.displayName)
			break
		case CLEAR_SE_MEDEWERKERS:
		case CLEAR_CACHE:
			break
		default:
			result = stateSlice
			break
	}

	return result
}

export default SeMedewerkersReducer
