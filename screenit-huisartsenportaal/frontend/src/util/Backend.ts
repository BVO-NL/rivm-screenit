/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-frontend
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
import {AppThunkDispatch, store} from "../index"
import {createActionPushToast} from "../state/ToastsState"
import {ToastType} from "../state/datatypes/Toast"
import {createClearStateAction} from "../state"
import {isValidationResponseDtoArray, ValidatieResponseDto} from "../state/datatypes/dto/ValidatieResponseDto"
import {getCookie} from "./CookieUtil"
import ky, {HTTPError} from "ky"
import {HttpErrorDto} from "../state/datatypes/dto/HttpErrorDto"

const BASE_URL = "/api/v1/"

export const ScreenitBackend = ky.create({
	prefixUrl: BASE_URL,
	hooks: {
		beforeRequest: [
			request => {
				const auth = store.getState().auth

				if (auth) {
					if (request.headers && !request.headers.has("Authorization")) {
						request.headers.set("Authorization", `Bearer ${auth.token}`)
					}
				}
				const xsrfToken = getCookie("XSRF-TOKEN")
				if (xsrfToken) {
					request.headers.set("X-XSRF-TOKEN", xsrfToken)
				}
			},
		],
		beforeError: [
			async (error: HTTPError<HttpErrorDto | ValidatieResponseDto[]>) => {
				const oauth = store.getState().auth
				const dispatch = store.dispatch as AppThunkDispatch
				const responseBody = await error.response.json()
				if (responseBody) {
					if (isValidationResponseDtoArray(responseBody)) {
						for (const code of responseBody) {
							if (code.defaultMessage) {
								dispatch(createActionPushToast({type: ToastType.ERROR, message: code.defaultMessage}))
							}
						}
					} else if (responseBody.exception === "ExpiredJwtException") {
						if (oauth) {
							setTimeout(() => dispatch(createActionPushToast({
								type: ToastType.ERROR,
								message: "Uw sessie is verlopen. Om door te gaan dient u opnieuw in te loggen.",
							})))
							dispatch(createClearStateAction())
						}
					} else {
						dispatch(createActionPushToast({
							type: ToastType.ERROR,
							message: responseBody.message!,
						}))
					}
				} else if (error.response?.status === 500) {
					dispatch(createActionPushToast({type: ToastType.ERROR, message: "Er ging iets fout bij de communicatie met ScreenIT. Probeer het later nog eens."}))
				}

				return error
			},
		],
	},
})

export default ScreenitBackend
