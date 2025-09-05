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
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import type {OpgehaaldeDagenActions} from "../actions/OpgehaaldeDagenActions"
import {DAGLIJST_OPGEHAALD} from "../actions/OpgehaaldeDagenActions"
import {Reducer} from "redux"

const OpgehaaldeDagenReducer: Reducer<Set<string>, OpgehaaldeDagenActions | ClearCacheActions> = (stateSlice = new Set(), action) => {
	switch (action.type) {
		case DAGLIJST_OPGEHAALD:
			return new Set([...stateSlice, action.datum])
		case CLEAR_CACHE:
			return new Set()
		default:
			return stateSlice
	}
}

export default OpgehaaldeDagenReducer
