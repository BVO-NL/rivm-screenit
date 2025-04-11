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
import axios, {AxiosError, AxiosResponse, Method} from "axios"
import {AppThunkDispatch, store} from "../index"
import {createActionPushToast} from "../state/ToastsState"
import {ToastType} from "../state/datatypes/Toast"
import {BackendError} from "../state/datatypes/dto/BackendError"
import {createClearStateAction} from "../state"
import {isValidationResponseDtoArray, ValidatieResponseDto} from "../state/datatypes/dto/ValidatieResponseDto"
import {getCookie} from "./CookieUtil"

const BASE_URL = "/api/v1/"

export const ScreenitBackend = axios.create({baseURL: BASE_URL, allowAbsoluteUrls: false})

ScreenitBackend.interceptors.request.use(async (config) => {
	const auth = store.getState().auth

	if (auth) {
		if (config.headers && !config.headers.Authorization) {
			config.headers.Authorization = `Bearer ${auth.token}`
		}
	}
	const xsrfToken = getCookie("XSRF-TOKEN")
	if (xsrfToken) {
		config.headers["X-XSRF-TOKEN"] = xsrfToken
	}

	return config
})

ScreenitBackend.interceptors.response.use((response) => {
	return response
}, async (error: AxiosError<BackendError>) => {
	const oauth = store.getState().auth
	const dispatch = store.dispatch as AppThunkDispatch

	if (error.response?.data?.message) {
		if (error.response.data.exception === "ExpiredJwtException") {
			if (oauth) {
				setTimeout(() => dispatch(createActionPushToast({type: ToastType.ERROR, message: "Uw sessie is verlopen. Om door te gaan dient u opnieuw in te loggen."})))
				dispatch(createClearStateAction())
			}
		} else {
			dispatch(createActionPushToast({
				type: ToastType.ERROR,
				message: error.response.data.message,
			}))
		}
	} else if (error.response?.status === 500) {
		dispatch(createActionPushToast({type: ToastType.ERROR, message: "Er ging iets fout bij de communicatie met ScreenIT. Probeer het later nog eens."}))
	}
	return Promise.reject(error)
})

export function validatingRequest<T>(url: string, method: Method, data: any): (dispatch: AppThunkDispatch) => Promise<T> {
	return async (dispatch: AppThunkDispatch) => {
		return ScreenitBackend.request({
			url: url,
			method: method,
			data: data,
		}).then((response: AxiosResponse<T>) => {
			return response.data
		}).catch((error: AxiosError<ValidatieResponseDto[]>) => {
			if (isValidationResponseDtoArray(error.response?.data)) {
				for (const code of error.response?.data) {
					if (code.defaultMessage) {
						dispatch(createActionPushToast({type: ToastType.ERROR, message: code.defaultMessage}))
					}
				}
			}
			throw error
		})
	}
}

export default ScreenitBackend
