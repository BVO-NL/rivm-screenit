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
import type {Zorginstelling} from "../datatypes/Zorginstelling"

export type ZorginstellingActions = VulZorginstellingenAction;
export const VUL_ZORGINSTELLINGEN = "VUL_ZORGINSTELLINGEN"
export type VulZorginstellingenAction = {
	type: "VUL_ZORGINSTELLINGEN";
	zorginstellingen: Array<Zorginstelling>;
};
export const createActionVulZorginstellingen = (zorginstellingen: Array<Zorginstelling>): VulZorginstellingenAction => ({
	type: VUL_ZORGINSTELLINGEN,
	zorginstellingen: zorginstellingen,
})
