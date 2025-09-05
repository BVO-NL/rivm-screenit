/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import keycloak from "../utils/Keycloak"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import properties from "./backend.json"
import {transformDates} from "./DateTransformUtil"
import {showToast} from "./ToastUtil"
import httpStatus from "../datatypes/HttpStatus"
import {countRequest, countResponse} from "./SpinnerCounterUtil"
import {getCookie} from "./CookieUtil"
import ky, {HTTPError} from "ky"

const BASE_URL = "/api"

export const ScreenitBackend = ky.create({
	prefixUrl: BASE_URL,
	parseJson: text => transformDates(JSON.parse(text)),
	hooks: {
		beforeRequest: [
			request => {
				countRequest()
				if (keycloak?.token !== undefined && request.headers) {
					request.headers.set("Authorization", `Bearer ${keycloak.token}`)
				}

				const xsrfToken = getCookie("XSRF-TOKEN")
				if (xsrfToken) {
					request.headers.set("X-XSRF-TOKEN", xsrfToken)
				}
			},
		],
		afterResponse: [
			async (input, options, response) => {
				countResponse()
				return response
			},
		],
		beforeError: [
			(error: HTTPError) => {
				countResponse()
				if (error.response?.status !== httpStatus.NOT_MODIFIED && error.response?.status !== httpStatus.CONFLICT && error.response?.status !== httpStatus.NOT_FOUND) {
					showToast(undefined, properties.foutmelding, ToastMessageType.ERROR)
				}
				return error
			},
		],
	},
})

export default ScreenitBackend
