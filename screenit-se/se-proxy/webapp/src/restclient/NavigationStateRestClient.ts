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
import {baseUrl, createClientHeaders} from "../util/ApiUtil"
import {store} from "../Store"
import {MELDING_SESSIE_NIET_GELDIG, persistentErrorToast, showErrorToastWithoutAutoClose} from "../util/ToastUtil"
import {GEEN_IDENTIFICATIE, logoutClient} from "./AuthenticatieRestclient"
import {resetTimeout} from "../util/TimeoutUtil"
import {NavigationActions, RestoredNavigationAction} from "../actions/NavigationActions"

export const putNavigationState = (navigationAction: NavigationActions | RestoredNavigationAction): void => {
	resetTimeout()

	const environmentInfo = store.getState().environmentInfo
	if (!environmentInfo || !environmentInfo.nfcEnabled) {
		return
	}

	const session = store.getState().session

	if (session && session.yubikeyIdentificatie !== GEEN_IDENTIFICATIE) {
		const clientHeaders = createClientHeaders()
		const yubikeyIdentificatie = session.yubikeyIdentificatie
		fetch(`${baseUrl}navigationState/${yubikeyIdentificatie}`, {
			method: "PUT",
			headers: clientHeaders,
			body: JSON.stringify(navigationAction),
		}).then((response) => {
			if (response.ok) {
			} else if (response.status === 400) {
				console.log(`Uitloggen door ongeldige sessie: ${yubikeyIdentificatie}`)
				logoutClient()
				showErrorToastWithoutAutoClose(MELDING_SESSIE_NIET_GELDIG)
			} else if (response.status === 408) {
				console.log(`Uitloggen door inactiviteit: ${yubikeyIdentificatie}`)
				logoutClient()
				persistentErrorToast("Uitgelogd wegens inactiviteit.")
			}
		}).catch((error) => {
			console.log(`Er ontstond een probleem tijdens het uitvoeren van fetch bij navigeren: ${error.message}`)
		})
	}
}
