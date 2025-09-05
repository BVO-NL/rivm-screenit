/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-frontend
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
import {AppThunkDispatch} from "../index"
import ScreenitBackend from "../util/Backend"
import {createActionSetHuisarts} from "../state/HuisartsState"
import {HuisartsDto} from "../state/datatypes/dto/HuisartsDto"

export const fetchHuisarts = () => async (dispatch: AppThunkDispatch) => {
	const huisarts: HuisartsDto = await ScreenitBackend.get<HuisartsDto>("huisarts").json()
	if (huisarts) {
		dispatch(createActionSetHuisarts(huisarts))
	}
}

export const controleerHuisarts = (huisarts: HuisartsDto) => async (dispatch: AppThunkDispatch): Promise<HuisartsDto> => {
	return ScreenitBackend.put<HuisartsDto>("huisarts/controle", {json: huisarts}).json()
}

export const saveHuisarts = (huisarts: HuisartsDto) => async (dispatch: AppThunkDispatch): Promise<HuisartsDto> => {
	const response = await ScreenitBackend.put<HuisartsDto>("huisarts", {json: huisarts}).json()
	dispatch(createActionSetHuisarts(response))
	return response
}
