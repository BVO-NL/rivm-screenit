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
import type {Recht} from "../datatypes/Recht"
import {AutorisatieActions, SET_AUTORISATIE} from "../actions/AutorisatieActions"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {isAuthorized} from "../util/AutorisatieUtil"
import {Reducer} from "redux"

const defaultRecht: Recht = {
	inschrijven: true,
	onderzoeken: false,
	signaleren: false,
	kwaliteitsopname: false,
	connectiestatus: false,
}

const AutorisatieReducer: Reducer<Recht, AutorisatieActions | ClearCacheActions> = (stateSlice = defaultRecht, action) => {
	switch (action.type) {
		case SET_AUTORISATIE:
			return {
				inschrijven: isAuthorized(action.rechten.inschrijvenRecht),
				onderzoeken: isAuthorized(action.rechten.onderzoekenRecht),
				signaleren: isAuthorized(action.rechten.signalerenRecht),
				kwaliteitsopname: isAuthorized(action.rechten.kwaliteitsopnameRecht),
				connectiestatus: isAuthorized(action.rechten.connectiestatusRecht),
			}
		case CLEAR_CACHE:
			return defaultRecht
		default:
			return stateSlice
	}
}

export default AutorisatieReducer
