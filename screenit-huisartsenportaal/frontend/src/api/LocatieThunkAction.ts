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
import {AppThunkDispatch, store} from "../index"
import ScreenitBackend, {validatingRequest} from "../util/Backend"
import {AxiosResponse} from "axios"
import {LocatieDto, LocatieStatus} from "../state/datatypes/dto/LocatieDto"
import {LocatieVerificatieDto} from "../state/datatypes/dto/LocatieVerificatieDto"
import {createActionSetLocatieVerificatie} from "../state/LocatieVerificatieState"
import {LocatieVerificatieResponseDto} from "../state/datatypes/dto/LocatieVerificatieResponseDto"
import {createActionSetLocaties} from "../state/LocatiesState"
import {fetchHuisarts} from "./HuisartsThunkAction"

export const fetchLocatieVerificatie = () => async (dispatch: AppThunkDispatch) => {
	const response: AxiosResponse<LocatieVerificatieDto[]> = await ScreenitBackend.get("/verificatie/locaties")
	const locatieDtos = response.data
	dispatch(createActionSetLocatieVerificatie(locatieDtos))
}

export const fetchLocaties = (status: LocatieStatus) => async (dispatch: AppThunkDispatch) => {
	const response = await ScreenitBackend.post("/locaties", {resultOptions: {first: 0, count: 10, sortOptions: {}}, status: status})
	const locaties = response.data
	dispatch(createActionSetLocaties({values: locaties, filter: status}))
}

export const verifieerLocatie = (locatie: LocatieVerificatieDto) => async (dispatch: AppThunkDispatch) => {
	const response: AxiosResponse<LocatieVerificatieResponseDto> = await ScreenitBackend.post("/verificatie/verifieerLocatie", locatie)
	if (response.data.succes) {
		await dispatch(fetchLocatieVerificatie())
		await dispatch(fetchLocaties(store.getState().locaties.filter))
		await dispatch(fetchHuisarts())
	}
}

export const herzendVerificatieCode = (locatie: LocatieVerificatieDto) => () => {
	return ScreenitBackend.post("/verificatie/herzendVerificatieCode", locatie)
}

export const putLocatie = (locatie: LocatieDto) => async (dispatch: AppThunkDispatch): Promise<LocatieDto> => {
	const response: LocatieDto = await dispatch(validatingRequest<LocatieDto>("/locatie", "PUT", locatie))
	await dispatch(fetchLocatieVerificatie())
	await dispatch(fetchLocaties(store.getState().locaties.filter))
	return response
}
