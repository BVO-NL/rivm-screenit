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
import type {MammografenStatus} from "../datatypes/connectiestatus/MammografenStatus"
import type {MammografenStatusActions} from "../actions/MammografenStatusActions"
import {VUL_MAMMOGRAFEN_STATUS} from "../actions/MammografenStatusActions"
import {Reducer} from "redux"

const MammografenStatusReducer: Reducer<MammografenStatus, MammografenStatusActions> = (stateSlice: MammografenStatus = [], action: MammografenStatusActions) => {
	switch (action.type) {
		case VUL_MAMMOGRAFEN_STATUS:
			return action.statusList
		default:
			return stateSlice
	}
}

export default MammografenStatusReducer
