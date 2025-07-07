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
import {Id, toast} from "react-toastify"

export const MELDING_SESSIE_NIET_GELDIG = "Uw sessie is niet geldig. Sluit alle ScreenIT vensters, leg Yubikey opnieuw op de lezer en log opnieuw in."
export const MELDING_TECHNISCHE_FOUT = "Er is een technische fout opgetreden."
export const MELDING_DUBBELE_INSTANTIE = "Er is al een ScreenIT tabblad open, sluit dit tabblad."
export const MELDING_YUBIKEY_GEDETECTEERD = "Yubikey gedetecteerd, log in om uw sessie te starten."
export const MELDING_NFC_ERROR = "Communicatiefout met Yubikey-lezer. Herstart werkstation."
export const MELDING_WIJZIGINGEN_VERWERKT = "Wijzigingen verwerkt."
export const DEFAULT_TOAST_TIMEOUT = 5000

let toastIdWijzigingenOpgeslagen: Id | undefined = undefined
let toastIdYubikeyHerkend: Id | undefined = undefined
let toastIdError: Id | undefined = undefined
let toastIdWarning: Id | undefined = undefined
let toastIdNfcError: Id | undefined = undefined
const MINUUT = 60000

const dismissToastIfActive = (toastId?: Id): void => {
	if (toastId && toast.isActive(toastId)) {
		toast.dismiss(toastId)
	}
}

export const dismissYubikeyHerkendToast = (): void => {
	dismissToastIfActive(toastIdYubikeyHerkend)
}

export const dismissNfcErrorToast = (): void => {
	dismissToastIfActive(toastIdNfcError)
}

export const dismissAllToasts = (): void => {
	dismissToastIfActive(toastIdWijzigingenOpgeslagen)
	dismissYubikeyHerkendToast()
	dismissNfcErrorToast()
	dismissToastIfActive(toastIdError)
	dismissToastIfActive(toastIdWarning)

}

export const showWijzigingenOpgeslagenToast = (): void => {
	if (!toastIdWijzigingenOpgeslagen || !toast.isActive(toastIdWijzigingenOpgeslagen)) {
		toastIdWijzigingenOpgeslagen = toast.success(MELDING_WIJZIGINGEN_VERWERKT, {
			type: "success",
			theme: "colored",
		})
	}
}

export const showYubikeyHerkendToast = (): void => {
	if (!toastIdYubikeyHerkend || !toast.isActive(toastIdYubikeyHerkend)) {
		toastIdYubikeyHerkend = toast.info(MELDING_YUBIKEY_GEDETECTEERD, {
			type: "info",
			theme: "colored",
			autoClose: false,
		})
	}
}

export const showNfcErrorToast = (): void => {
	dismissYubikeyHerkendToast()

	if (!toastIdNfcError || !toast.isActive(toastIdNfcError)) {
		toastIdNfcError = toast.error(MELDING_NFC_ERROR, {
			type: "error",
			theme: "colored",
			autoClose: false,
		})
	}
}

export const showWarningToast = (message: string, viewTime?: number): void => {
	if (!toastIdWarning || !toast.isActive(toastIdWarning)) {
		toastIdWarning = toast.warn(message, {
			autoClose: viewTime ? viewTime : MINUUT,
			theme: "colored",
			type: "warning",
		})
	}
}

export const showErrorToast = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	if (!toastIdError || !toast.isActive(toastIdError)) {
		toastIdError = toast.error(message, {
			type: "error",
			theme: "colored",
		})
	}
}

export const showErrorToastWithoutAutoClose = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	if (!toastIdError || !toast.isActive(toastIdError)) {
		toastIdError = toast.error(message, {
			type: "error",
			theme: "colored",
			autoClose: false,
		})
	}
}

export const persistentErrorToast = (message: string): void => {
	console.warn(`Toast met foutmelding '${message}'`)
	toast.error(message, {
		type: "error",
		theme: "colored",
	})
}

export const persistentSuccessToast = (message: string): void => {
	toast.success(message, {
		type: "success",
		theme: "colored",
	})
}
