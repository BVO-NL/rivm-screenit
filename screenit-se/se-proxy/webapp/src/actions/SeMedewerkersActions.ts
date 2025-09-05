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
export type SeMedewerkersActions = AddAllSeMedewerkersAction | AddSeMedewerkerAction | ClearSeMedewerkersAction;
export const ADD_ALL_SE_MEDEWERKERS = "ADD_ALL_SE_MEDEWERKERS"
export type AddAllSeMedewerkersAction = {
	type: "ADD_ALL_SE_MEDEWERKERS";
	seMedewerkers: Map<number, string>;
};
export const createActionAddAllSeMedewerkers = (seMedewerkers: Map<number, string>): AddAllSeMedewerkersAction => {
	return {
		type: ADD_ALL_SE_MEDEWERKERS,
		seMedewerkers: seMedewerkers,
	}
}
export const ADD_SE_MEDEWERKER = "ADD_SE_MEDEWERKER"
export type AddSeMedewerkerAction = {
	type: "ADD_SE_MEDEWERKER";
	organisatieMedewerkerId: string;
	displayName: string;
};
export const CLEAR_SE_MEDEWERKERS = "CLEAR_SE_MEDEWERKERS"
export type ClearSeMedewerkersAction = {
	type: "CLEAR_SE_MEDEWERKERS";
};
