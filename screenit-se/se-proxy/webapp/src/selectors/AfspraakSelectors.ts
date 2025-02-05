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
import {RootState} from "../Store"
import {Afspraak, Afspraakstatus} from "../datatypes/Afspraak"
import {
	getAantalAfsprakenUitDagverslag,
	getAantalBeeindigdeAfsprakenUitDagverslag,
	getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag,
	getAantalOnderzoekenMetStatusUitDagverslag,
} from "./DagverslagSelector"
import {Onderzoekstatus} from "../datatypes/Onderzoek"
import {heeftAfspraakOnderzoekStatus} from "./OnderzoekSelector"

export const getDagAfspraken = (state: RootState): Array<Afspraak> => {
	return [...state.afsprakenById.values()].filter((afspraak) => afspraak.vanafDatum === state.daglijstDatum)
}

export const getAantalNietBeeindigdeAfsprakenMetStatus = (state: RootState, status: Exclude<Afspraakstatus, "BEEINDIGD" | "KWALITEITSOPNAME">): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken.filter((afspraak) => afspraak.status === status).length
	}
	return getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag(state, status)
}

export const getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus = (state: RootState, status: Exclude<Onderzoekstatus, "ACTIEF">): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken
			.filter((afspraak) => afspraak.status === "BEEINDIGD"
				&& heeftAfspraakOnderzoekStatus(state, afspraak, status)).length
	}
	return getAantalOnderzoekenMetStatusUitDagverslag(state, status)
}

export const getTotaalAfsprakenAfgerondDag = (state: RootState): number => {
	const dagAfspraken = getDagAfspraken(state)
	if (dagAfspraken.length !== 0) {
		return dagAfspraken.filter((afspraak) => afspraak.status === "BEEINDIGD").length
	}
	return getAantalBeeindigdeAfsprakenUitDagverslag(state)
}

export const getTotaalAfsprakenDag = (state: RootState): number => {
	const dagAfspraken = getDagAfspraken(state)
	return dagAfspraken.length !== 0 ? dagAfspraken.length : getAantalAfsprakenUitDagverslag(state)
}
