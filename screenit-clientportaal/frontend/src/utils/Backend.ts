/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import ky, {AfterResponseState, BeforeErrorState, BeforeRequestState, isHTTPError, KyRequest, KyResponse} from "ky"

const BASE_URL = "/api"
const statussen = [httpStatus.NOT_MODIFIED, httpStatus.NOT_FOUND, httpStatus.CONFLICT, httpStatus.UNPROCESSABLE_ENTITY, httpStatus.ACCEPTED]

export async function vervangLegeBodyDoorNull(response: KyResponse): Promise<KyResponse> {
	const clone = response.clone()
	const text = await clone.text()
	if (text === "") {
		response.text = async () => "null"
	}
	return response
}

export const ScreenitBackend = ky.create({
	prefix: BASE_URL,
	parseJson: text => transformDates(JSON.parse(text)),
	hooks: {
		beforeRequest: [
			({request}: BeforeRequestState): KyRequest => {
				countRequest()
				if (keycloak?.token !== undefined && request.headers) {
					request.headers.set("Authorization", `Bearer ${keycloak.token}`)
				}

				const xsrfToken = getCookie("XSRF-TOKEN")
				if (xsrfToken) {
					request.headers.set("X-XSRF-TOKEN", xsrfToken)
				}
				return request
			},
		],
		afterResponse: [
			async ({response}: AfterResponseState): Promise<KyResponse> => {
				countResponse()
				return vervangLegeBodyDoorNull(response)
			},
		],
		beforeError: [
			({error}: BeforeErrorState): Error => {
				if (isHTTPError(error) && !statussen.includes(error.response.status)) {
					showToast(undefined, properties.foutmelding, ToastMessageType.ERROR)
				}
				countResponse()
				return error
			},
		],
	},
})

export default ScreenitBackend
