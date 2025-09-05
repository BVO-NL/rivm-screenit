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
import {Afspraakstatus} from "../datatypes/Afspraak"
import {Onderzoekstatus} from "../datatypes/Onderzoek"
import {DagPlanningSamenvatting} from "../datatypes/Dagverslag"

export const getAantalNietBeeindigdeAfsprakenMetStatusUitDagverslag = (state: RootState, status: Exclude<Afspraakstatus, "KWALITEITSOPNAME" | "BEEINDIGD">): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	return dagSamenvatting && status === "VERWACHT" ? dagSamenvatting.aantalVerwacht : 0
}

export const getAantalOnderzoekenMetStatusUitDagverslag = (state: RootState, status: Exclude<Onderzoekstatus, "ACTIEF">): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		switch (status) {
			case "ONDERBROKEN":
				return dagSamenvatting.aantalOnderbroken
			case "ONVOLLEDIG":
				return dagSamenvatting.aantalOnvolledig
			case "AFGEROND":
				return dagSamenvatting.aantalAfgerond
		}
	}
	return 0
}

export const getAantalBeeindigdeAfsprakenUitDagverslag = (state: RootState): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		return dagSamenvatting.aantalAfgerond + dagSamenvatting.aantalOnvolledig + dagSamenvatting.aantalOnderbroken
	}
	return 0
}

export const getAantalAfsprakenUitDagverslag = (state: RootState): number => {
	const dagSamenvatting = getDagSamenvattingVanDag(state)
	if (dagSamenvatting) {
		return dagSamenvatting.aantalVerwacht + dagSamenvatting.aantalAfgerond + dagSamenvatting.aantalOnvolledig + dagSamenvatting.aantalOnderbroken
	}
	return 0
}

const getDagSamenvattingVanDag = (state: RootState): DagPlanningSamenvatting | undefined => {
	return state.dagverslag.get(state.daglijstDatum)?.dagPlanningSamenvatting
}
