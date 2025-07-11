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
import type {Dagverslag} from "../datatypes/Dagverslag"

export type DagverslagActions = SetDagverslagAction | SetNietAfgeslotenVanafAction;

export const SET_DAGVERSLAG = "SET_DAGVERSLAG"
export type SetDagverslagAction = {
	type: "SET_DAGVERSLAG";
	datum: string;
	dagverslag: Dagverslag;
};
export const createActionSetDagverslag = (datum: string, dagverslag: Dagverslag): SetDagverslagAction => {
	return {
		type: SET_DAGVERSLAG,
		datum: datum,
		dagverslag: dagverslag,
	}
}

export const SET_NIET_AFGESLOTEN_DATUM = "SET_NIET_AFGESLOTEN_DATUM"
export type SetNietAfgeslotenVanafAction = {
	type: "SET_NIET_AFGESLOTEN_DATUM";
	nietAfgeslotenVanaf?: string;
};
export const createActionSetNietAfgeslotenVanaf = (datum?: string): SetNietAfgeslotenVanafAction => {
	return {
		type: SET_NIET_AFGESLOTEN_DATUM,
		nietAfgeslotenVanaf: datum,
	}
}
