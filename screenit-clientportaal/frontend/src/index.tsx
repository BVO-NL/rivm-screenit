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
import * as React from "react"
import * as serviceWorker from "./serviceWorker"
import "./index.css"
import {Action, applyMiddleware, compose, createStore, Store} from "redux"
import cpReducers from "./reducers"
import {loadState, saveState} from "./utils/StorageUtil"
import {Provider, useDispatch} from "react-redux"
import {BrowserRouter} from "react-router"
import App from "./App"
import {thunk, ThunkDispatch} from "redux-thunk"
import {State} from "./datatypes/State"
import IdleTimerWrapper from "./wrapper/IdleTimerWrapper"
import {datadogRum} from "@datadog/browser-rum"
import AuthenticationWrapper from "./wrapper/AuthenticationWrapper"
import {createRoot} from "react-dom/client"
import createCache from "@emotion/cache"
import {CacheProvider} from "@emotion/react"
import ErrorBoundary from "./components/error_boundary/ErrorBoundary"
import KeycloakProvider from "./components/KeycloakProvider"
import {setLoggingOutAction} from "./actions/AuthenticatieAction"

export type ReduxThunkDispatch = ThunkDispatch<State, any, Action>;

export function useThunkDispatch(): ReduxThunkDispatch {
	return useDispatch<ReduxThunkDispatch>()
}

const composeEnhancers =
	(process.env.NODE_ENV !== "production" &&
		(window as any).__REDUX_DEVTOOLS_EXTENSION_COMPOSE__) ||
	compose

export const cpStore: Store = createStore(
	cpReducers,
	loadState(),
	composeEnhancers(applyMiddleware(thunk)),
)
cpStore.subscribe(() => {
	saveState()
})

const isAcceptatie = window.location.hostname === "acc.mijn.bevolkingsonderzoeknederland.nl"
const isProductie = window.location.hostname === "mijn.bevolkingsonderzoeknederland.nl"

if (isAcceptatie || isProductie) {
	datadogRum.init({
		applicationId: isAcceptatie ? process.env.REACT_APP_DD_RUM_APPLICATION_ID_ACC : process.env.REACT_APP_DD_RUM_APPLICATION_ID_PROD,
		clientToken: isAcceptatie ? process.env.REACT_APP_DD_RUM_CLIENT_TOKEN_ACC : process.env.REACT_APP_DD_RUM_CLIENT_TOKEN_PROD,
		site: "datadoghq.eu",
		service: "clientportaal",
		sessionReplaySampleRate: 0,
		trackUserInteractions: false,
		defaultPrivacyLevel: "mask-user-input",
	})
}

interface MetaElement extends HTMLElement {
	content: string;
}

const emotionCache = createCache({
	key: "emotion-cache",
	nonce:
		(document.querySelector("meta[property=\"csp-nonce\"]") as MetaElement)
			.content || "",
})
const component = document.getElementById("root")
const root = createRoot(component!)

root.render(
	<KeycloakProvider
		onAuthRefreshError={() => cpStore.dispatch(setLoggingOutAction(true))}
	>
		<ErrorBoundary>
			<Provider store={cpStore}>
				<CacheProvider value={emotionCache}>
					<BrowserRouter>
						<AuthenticationWrapper>
							<IdleTimerWrapper>
								<App/>
							</IdleTimerWrapper>
						</AuthenticationWrapper>
					</BrowserRouter>
				</CacheProvider>
			</Provider>
		</ErrorBoundary>
	</KeycloakProvider>,
)

serviceWorker.unregister()
