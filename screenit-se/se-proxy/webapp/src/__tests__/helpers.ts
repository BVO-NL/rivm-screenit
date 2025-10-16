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
import {RootState} from "../Store"
import {WEBSOCKET_STATUS_OFFLINE} from "../datatypes/WebsocketStatus"

export const createStateHelper = (stateSlice: Partial<RootState> = {}): RootState => {
	return {
		activeStudyForIms: null,
		afsprakenById: new Map(),
		autorisatie: {inschrijven: false, connectiestatus: false, kwaliteitsopname: false, onderzoeken: false, signaleren: false},
		bezigMetKwaliteitsopnameVolgnr: 0,
		clientenById: new Map(),
		connectieStatus: {
			imsConnectieStatus: "OK",
			imsConnectieStatusTimestamp: "",
			mammograafConnectieStatusByAeTitle: new Map(),
		},
		daglijstDatum: "01-01-2023",
		dagverslag: new Map(),
		dubbeleInstantie: false,
		environmentInfo: {
			dagenDaglijstOphalenLimiet: 0,
			environment: "Test",
			huidigWerkstationIpAdres: "",
			nfcEnabled: true, timestamp: "",
			tomosyntheseMogelijk: true,
			version: "23.4",
		},
		error: {errorReferentie: ""},
		formsByFormId: new Map(),
		heeftWijzigingen: false,
		huidigeMammograafId: 0,
		huisartsenById: new Map(),
		loginStatus: {inlogActief: false, stopIdentificerenTotYubikeyEraf: false},
		mammografenById: new Map(),
		mammografenStatus: [],
		navigation: {
			afspraakId: 0,
			clientId: 0,
			subPagina: undefined,
			tab: "Daglijst",
		},
		nietAfgeslotenVanaf: null,
		onderzoekByAfspraakId: new Map(),
		online: false,
		opgehaaldeDagen: new Set<string>(),
		pendingUpdates: null,
		planning: new Map(),
		popup: {
			akkoordString: "",
			alleenOnline: false,
			annulerenString: "",
			body: undefined,
			callback(args: unknown): void {
			},
			cancelCallback(args: unknown): void {
			},
			titel: "", visible: false,
		},
		seMedewerkers: new Map(),
		session: null,
		signaleringByAfspraakId: new Map(),
		visueleInspectieAfbeeldingByAfspraakId: new Map(),
		zorginstellingen: new Map(),
		websocketStatus: WEBSOCKET_STATUS_OFFLINE,
		...stateSlice,
	}
}
