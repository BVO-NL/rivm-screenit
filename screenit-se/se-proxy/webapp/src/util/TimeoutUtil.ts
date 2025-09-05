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
import {store} from "../Store"
import {logoutClient} from "../restclient/AuthenticatieRestclient"
import {showErrorToast} from "./ToastUtil"
import {nu} from "./DateUtil"
import {Moment} from "moment"

let logoutMoment: Moment
let idleCheck: NodeJS.Timeout
export const ensureIdleCheck = (): void => {
	if (idleCheck) {
		clearInterval(idleCheck)
	}

	resetTimeout()
	idleCheck = setInterval(timerIncrement, 1000)
}
export const resetTimeout = (): void => {
	logoutMoment = nu().add(30, "minutes")
}

const timerIncrement = (): void => {
	const session = store.getState().session
	if (session && store.getState().online && nu() > logoutMoment) {
		const yubikeyIdentificatie = session.yubikeyIdentificatie
		console.log(`Uitloggen door 30 min inactiviteit: ${yubikeyIdentificatie}`)
		logoutClient()
		showErrorToast("Uitgelogd wegens inactiviteit")
	}
}
