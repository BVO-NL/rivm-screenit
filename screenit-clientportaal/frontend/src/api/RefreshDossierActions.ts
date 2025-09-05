/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import {Dispatch} from "redux"
import ScreenitBackend from "../utils/Backend"
import {createColonDossierAction} from "../actions/ColonDossierAction"
import {createCervixDossierAction} from "../actions/CervixDossierAction"
import {createMammaDossierAction} from "../actions/MammaDossierAction"
import {ColonDossier} from "../datatypes/ColonDossier"
import {CervixDossier} from "../datatypes/CervixDossier"
import {MammaDossier} from "../datatypes/MammaDossier"

export const refreshColonDossier = () => (dispatch: Dispatch) => {

	return ScreenitBackend.get<ColonDossier>("dossier/colon").json()
		.then((response) => dispatch(createColonDossierAction(response)))
}

export const refreshCervixDossier = () => (dispatch: Dispatch) => {

	return ScreenitBackend.get<CervixDossier>("dossier/cervix").json()
		.then(response => dispatch(createCervixDossierAction(response)))
}

export const refreshMammaDossier = () => (dispatch: Dispatch) => {

	return ScreenitBackend.get<MammaDossier>("dossier/mamma").json()
		.then(response => dispatch(createMammaDossierAction(response)))
}
