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
import {getMandatory} from "./MapUtil"
import {store} from "../Store"

export const calcSort = (value1: string, value2: string): number => value1 === value2 ? 0 : value1 > value2 ? 1 : -1
export const disablePrimarySeKnop = (): void => {
	const primarySeKnop = document.getElementById("Popover-btn") as HTMLButtonElement

	if (primarySeKnop) {
		primarySeKnop.disabled = true
	}
}

export const aeTitle = (): string => {
	const huidigeMammograafId = store.getState().huidigeMammograafId
	if (huidigeMammograafId) {
		return getMandatory(store.getState().mammografenById, huidigeMammograafId).aeTitle
	} else {
		return ""
	}
}

export const seCode = (): string => {
	if (aeTitle().length === 0 || aeTitle().split("-").length === 0) {
		return ""
	}
	return aeTitle().substr(aeTitle().split("-")[0].length + 1, 6)
}
