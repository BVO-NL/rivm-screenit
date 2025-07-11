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
import type {SeGebruikersActions} from "../actions/SeGebruikersActions"
import {ADD_ALL_SE_GEBRUIKERS, ADD_SE_GEBRUIKER, CLEAR_SE_GEBRUIKERS} from "../actions/SeGebruikersActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const SeGebruikersReducer: Reducer<Map<string, string>, SeGebruikersActions | ClearCacheActions> = (stateSlice = new Map(), action) => {
	let result = new Map()
	if (!stateSlice) {
		stateSlice = result
	}
	switch (action.type) {
		case ADD_ALL_SE_GEBRUIKERS:
			result = new Map([...stateSlice, ...Object.entries(action.seGebruikers)])
			break
		case ADD_SE_GEBRUIKER:
			result = stateSlice
			result.set(String(action.instellingGebruikerId), action.displayName)
			break
		case CLEAR_SE_GEBRUIKERS:
		case CLEAR_CACHE:
			break
		default:
			result = stateSlice
			break
	}

	return result
}

export default SeGebruikersReducer
