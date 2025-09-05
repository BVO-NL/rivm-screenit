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
import {fetchWithTimeout} from "../util/FetchUtil"

export type NfcVersieResponse = {
	nfcServerVersie: string;
}

export type NfcOtpResponse = {
	public_id: string;
	otp: string;
}

const fetchTimeoutNfcService = 5000
const nfcBaseUrl = "http:
export const getVersie = (): Promise<NfcVersieResponse> => {
	const environmentInfo = store.getState().environmentInfo
	if (environmentInfo && !environmentInfo.nfcEnabled) {
		return new Promise(resolve => {
			resolve({
				nfcServerVersie: "NFC-disabled",
			})
		})
	}

	return fetchWithTimeout(`${nfcBaseUrl}versie`, fetchTimeoutNfcService, {
		method: "GET",
		mode: "cors",
	}).then((response) => {
		if (response.status === 200) {
			return response.json().then((responseData) => {
				return responseData
			})
		}
	}).catch((error) => {
		console.error(`Communicatiefout met NFC lezer bij getVersie: ${error}`)
	})
}

export const readNFC = (): Promise<NfcOtpResponse> => {
	const environmentInfo = store.getState().environmentInfo
	if (environmentInfo && !environmentInfo.nfcEnabled) {
		return new Promise(resolve => {
			resolve({
				otp: "GEEN_OTP",
				public_id: "GEEN_IDENTIFICATIE",
			})
		})
	}

	return fetchWithTimeout(`${nfcBaseUrl}nfc`, fetchTimeoutNfcService, {
		method: "GET",
		mode: "cors",
	}).then((response) => {
		if (response.status === 200) {
			return response.json().then((responseData) => {
				return responseData
			})
		}
	})
}
