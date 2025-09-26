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
import {datadogRum, RumInitConfiguration} from "@datadog/browser-rum"
import {AnalyticsCategorie} from "../datatypes/AnalyticsCategorie"

const defaultRumInitConfiguration: Partial<RumInitConfiguration> = {
	site: "datadoghq.eu",
	service: "clientportaal",
	sessionReplaySampleRate: 0,
	trackUserInteractions: false,
	defaultPrivacyLevel: "mask-user-input",
}

class DatadogService {
	init(): void {
		const isAcceptatie = window.location.hostname === "acc.mijn.bevolkingsonderzoeknederland.nl"
		const isProductie = window.location.hostname === "mijn.bevolkingsonderzoeknederland.nl"

		if (isAcceptatie || isProductie) {
			datadogRum.init({
				...defaultRumInitConfiguration,
				applicationId: isAcceptatie ? process.env.REACT_APP_DD_RUM_APPLICATION_ID_ACC : process.env.REACT_APP_DD_RUM_APPLICATION_ID_PROD,
				clientToken: isAcceptatie ? process.env.REACT_APP_DD_RUM_CLIENT_TOKEN_ACC : process.env.REACT_APP_DD_RUM_CLIENT_TOKEN_PROD,
			})
		}
	}

	stuurEvent(name: string, categorie: AnalyticsCategorie, attributes?: Record<string, unknown>): void {
		const context = {
			categorie: categorie,
			...attributes,
		}
		datadogRum.addAction(name, context)
	}
}

const datadogService: DatadogService = new DatadogService()
export default datadogService
