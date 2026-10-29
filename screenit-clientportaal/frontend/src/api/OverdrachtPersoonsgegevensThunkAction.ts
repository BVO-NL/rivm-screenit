/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {createOverdrachtPersoonsgegevensAction, OverdrachtPersoonsgegevensAction} from "../actions/OverdrachtPersoonsgegevensAction"
import {legeOverdrachtPersoonsgegevens, OverdrachtPersoonsgegevens} from "../datatypes/OverdrachtPersoonsgegevens"
import HttpStatus from "../datatypes/HttpStatus"

export const getOverdrachtPersoonsgegevens =
	() => (dispatch: Dispatch<OverdrachtPersoonsgegevensAction>): Promise<void | OverdrachtPersoonsgegevensAction> => {
		return ScreenitBackend.get<OverdrachtPersoonsgegevens>(
			"overdracht-persoonsgegevens",
		)
			.json()
			.then((response) =>
				dispatch(createOverdrachtPersoonsgegevensAction(response)),
			)
			.catch((error) => {
				if (error.response.status === HttpStatus.NOT_FOUND) {
					dispatch(createOverdrachtPersoonsgegevensAction(legeOverdrachtPersoonsgegevens))
				}
			})
	}
