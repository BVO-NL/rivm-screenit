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
import type {ConnectieStatus, ConnectieStatusLevel} from "../datatypes/connectiestatus/ConnectieStatus"
import type {ConnectieStatusActions} from "../actions/ConnectieStatusActions"
import {PUT_IMS_CONNECTIE_STATUS, PUT_MAMMOGRAAF_CONNECTIE_STATUS} from "../actions/ConnectieStatusActions"
import {nuISO} from "../util/DateUtil"
import {Reducer} from "redux"

const defaultConnectieStatus: ConnectieStatus = {
	mammograafConnectieStatusByAeTitle: new Map(),
	imsConnectieStatus: "WARN",
	imsConnectieStatusTimestamp: undefined,
}

const ConnectieStatusReducer: Reducer<ConnectieStatus, ConnectieStatusActions> = (stateSlice = defaultConnectieStatus, action) => {
	switch (action.type) {
		case PUT_MAMMOGRAAF_CONNECTIE_STATUS:
			const clone: Map<string, ConnectieStatusLevel> = new Map(stateSlice.mammograafConnectieStatusByAeTitle)
			clone.set(action.aeTitle, action.statusLevel)
			return {
				mammograafConnectieStatusByAeTitle: clone,
				imsConnectieStatus: stateSlice.imsConnectieStatus,
				imsConnectieStatusTimestamp: stateSlice.imsConnectieStatusTimestamp,
			}
		case PUT_IMS_CONNECTIE_STATUS:
			return {
				mammograafConnectieStatusByAeTitle: stateSlice.mammograafConnectieStatusByAeTitle,
				imsConnectieStatus: action.statusLevel,
				imsConnectieStatusTimestamp: nuISO(),
			}
		default:
			return stateSlice
	}
}

export default ConnectieStatusReducer
