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
import type {RechtMetGeldigheid} from "../datatypes/RechtMetGeldigheid"

export type AutorisatieActions = SetAutorisatieAction;
export const SET_AUTORISATIE = "SET_AUTORISATIE"
export type SetAutorisatieAction = {
	type: "SET_AUTORISATIE";
	rechten: RechtMetGeldigheid;
};
export const createActionSetAutorisatie = (recht: RechtMetGeldigheid): SetAutorisatieAction => {
	return {
		type: SET_AUTORISATIE,
		rechten: recht,
	}
}
