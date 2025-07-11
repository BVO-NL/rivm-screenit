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
import {store} from "../Store"
import {fetchApiPromise} from "../util/ApiUtil"
import {createActionVulMammografenStatus} from "../actions/MammografenStatusActions"
import type {MammograafStatus} from "../datatypes/connectiestatus/MammograafStatus"
import {createActionPutMammograafConnectieStatus} from "../actions/ConnectieStatusActions"
import {Mammograaf} from "../datatypes/Mammograaf"

export const readMammografenStatus = (mammografen: Array<Mammograaf>): Promise<void> => {
	return new Promise(resolve => {
		fetchApiPromise("GET", `mammografenstatus?aeTitles=${encodeURIComponent(mammografen.map(m => m.aeTitle).toString())}`).then(response => {
			response.json().then((statusList: Array<MammograafStatus>) => {
				store.dispatch(createActionVulMammografenStatus(statusList))
				statusList.forEach(m => store.dispatch(createActionPutMammograafConnectieStatus(m.aeTitle, "WARN")))
				resolve()
			})
		})
	})
}
