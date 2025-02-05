/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

export type ConsoleMelding = {
	level: "LOG" | "ERROR" | "TRACE" | "WARN";
	melding: string;
	stack?: string;
}

export const verstuurConsoleMeldingNaarCentraal = (melding: ConsoleMelding): void => {
	const session = store.getState().session
	const medewerkerCode = session ? session.medewerkercode : "onbekend"
	fetch(`${baseUrl}console-melding/${medewerkerCode}`, {
		method: "POST",
		headers: createClientHeaders(),
		body: JSON.stringify({
			level: melding.level,
			melding: encodeURIComponent(melding.melding),
			stack: melding.stack,
		}),
	})
}
