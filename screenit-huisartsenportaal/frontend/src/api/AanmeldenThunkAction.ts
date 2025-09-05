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
import {createActionSetAuth} from "../state/AuthState"
import {fetchCurrentUser} from "./CurrentUserThunkAction"
import {fetchHuisarts} from "./HuisartsThunkAction"
import {fetchLocaties, fetchLocatieVerificatie} from "./LocatieThunkAction"
import {LocatieStatus} from "../state/datatypes/dto/LocatieDto"
import {createActionSetAuthenticationLoading} from "../state/AuthenticationLoadingState"
import {TokenDto} from "../state/datatypes/dto/TokenDto"
import {CredentialsDto} from "../state/datatypes/dto/CredentialsDto"

export const aanmelden = (loginDto: CredentialsDto) => async (dispatch: AppThunkDispatch) => {
	const token: TokenDto = await ScreenitBackend.post("auth/inloggen", {json: loginDto}).json()
	dispatch(createActionSetAuthenticationLoading(true))
	dispatch(createActionSetAuth(token))
	await dispatch(fetchCurrentUser())
	dispatch(createActionSetAuthenticationLoading(false))
	await dispatch(fetchHuisarts())
	await dispatch(fetchLocatieVerificatie())
	await dispatch(fetchLocaties(LocatieStatus.ACTIEF))

}
