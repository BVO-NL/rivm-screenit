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
import {createEmptyStudyIms, createLogoffIms, createLogonIms, createStudyIms} from "./ImsFactory"
import {store} from "../Store"
import {createActionClearStudyForIms, createActionSetStudyForIms} from "../actions/ImsActions"
import {createActionPutIMSConnectieStatus} from "../actions/ConnectieStatusActions"
import {fetchApiPromise} from "./ApiUtil"

const url = "https:

const getStandardHeaders = (): Headers => {
	const headers = new Headers()
	headers.append("Content-type", "application/json; charset=utf-8")
	return headers
}

const setStudyForIms = (uitnodigingsNr: number): void => {
	store.dispatch(createActionSetStudyForIms(uitnodigingsNr))
}

const clearStudyForIms = (): void => {
	store.dispatch(createActionClearStudyForIms())
}

export const sendStudyMessageToIMS = async (uitnodigingsNr: number, bsn: string, username: string): Promise<void> => {
	console.log("Study openen naar IMS")
	setStudyForIms(uitnodigingsNr)

	try {
		const response = await fetchApiPromise("POST", "ims/launchUrl/desktopSync", createLaunchUrlContext(username, bsn, uitnodigingsNr))
		const launchUrl = await response.text()
		return fetchIMS(createStudyIms(uitnodigingsNr, bsn, username, launchUrl))
	} catch {
		console.error("Fout bij ophalen desktopSync launchUrl van proxy")
	}
}

const createLaunchUrlContext = (username: string, bsn?: string, uitnodigingsNr?: number): string => {
	return JSON.stringify({
		gebruikersnaam: username,
		bsn: bsn,
		uitnodigingsNr: uitnodigingsNr,
	})
}

export const sendEmptyStudyMessageToIMS = async (username: string): Promise<void> => {
	console.log("Empty study naar IMS")
	clearStudyForIms()
	return fetchIMS(createEmptyStudyIms(username))
}

export const logonToIMS = async (username: string): Promise<void> => {
	console.log("Inloggen naar IMS")
	clearStudyForIms()

	try {
		const response = await fetchApiPromise("POST", "ims/launchUrl/login", createLaunchUrlContext(username))
		const launchUrl = await response.text()
		return fetchIMS(createLogonIms(username, launchUrl))
	} catch {
		console.error("Fout bij ophalen logon launchUrl van proxy")
	}
}

export const logoffToIMS = async (username: string): Promise<void> => {
	console.log("Uitloggen naar IMS")
	clearStudyForIms()
	return fetchIMS(createLogoffIms(username))
}

const fetchIMS = async (imsAction: any): Promise<void> => {
	try {
		await fetch(url, {
			method: "PUT",
			mode: "cors",
			headers: getStandardHeaders(),
			body: JSON.stringify(imsAction),
		})
		store.dispatch(createActionPutIMSConnectieStatus("OK"))
	} catch {
		store.dispatch(createActionPutIMSConnectieStatus("FAULT"))
	}
}
