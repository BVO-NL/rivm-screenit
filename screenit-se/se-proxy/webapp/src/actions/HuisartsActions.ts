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
import type {GeenHuisartsOption, Huisarts} from "../datatypes/Huisarts"

export type HuisartsActions = VulHuisartsenByIdAction | KiesHuisartsAction | KiesGeenHuisartsOptieAction;
export const VUL_HUISARTSEN_BY_ID = "VUL_HUISARTSEN_BY_ID"
export type VulHuisartsenByIdAction = {
	type: "VUL_HUISARTSEN_BY_ID";
	huisartsen: Array<Huisarts>;
};
export const createActionVulHuisartsenById = (huisartsen: Array<Huisarts>): VulHuisartsenByIdAction => {
	return {
		type: VUL_HUISARTSEN_BY_ID,
		huisartsen: huisartsen,
	}
}
export const KIES_HUISARTS = "KIES_HUISARTS"
export type KiesHuisartsAction = {
	type: "KIES_HUISARTS";
	afspraakId: number;
	huisartsId: number;
};
export const createActionKiesHuisarts = (afspraakId: number, huisartsId: number): KiesHuisartsAction => {
	return {
		type: KIES_HUISARTS,
		afspraakId,
		huisartsId,
	}
}
export const KIES_GEEN_HUISARTS_OPTIE = "KIES_GEEN_HUISARTS_OPTIE"
export type KiesGeenHuisartsOptieAction = {
	type: "KIES_GEEN_HUISARTS_OPTIE";
	afspraakId: number;
	geenHuisartsOptie: GeenHuisartsOption;
};
export const createActionKiesGeenHuisartsOptie = (afspraakId: number, geenHuisartsOptie: GeenHuisartsOption): KiesGeenHuisartsOptieAction => {
	return {
		type: KIES_GEEN_HUISARTS_OPTIE,
		afspraakId,
		geenHuisartsOptie,
	}
}
