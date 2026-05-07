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
import {AfspraakOptie} from "../datatypes/mamma/AfspraakOptie"
import {AfspraakBevestigingOpties} from "../datatypes/mamma/AfspraakBevestigingOpties"

export type MammaAfspraakActions =
	SetMammaAfspraak
	| SetMammaAfspraakBevestigingsoptie

export const SET_MAMMA_AFSPRAAK = "SET_MAMMA_AFSPRAAK"
export type SetMammaAfspraak = { type: typeof SET_MAMMA_AFSPRAAK, afspraakOptie: AfspraakOptie | undefined }
export const setMammaAfspraakReduxAction = (afspraakOptie: AfspraakOptie | undefined): SetMammaAfspraak => ({
	type: SET_MAMMA_AFSPRAAK,
	afspraakOptie,
})

export const SET_MAMMA_AFSPRAAK_BEVESTIGINGSOPTIE = "SET_MAMMA_AFSPRAAK_BEVESTIGINGSOPTIE"
export type SetMammaAfspraakBevestigingsoptie = { type: typeof SET_MAMMA_AFSPRAAK_BEVESTIGINGSOPTIE, afspraakBevestigingsoptie: AfspraakBevestigingOpties | undefined }
export const setMammaAfspraakBevestigingsoptieReduxAction = (afspraakBevestigingsoptie: AfspraakBevestigingOpties | undefined): SetMammaAfspraakBevestigingsoptie => ({
	type: SET_MAMMA_AFSPRAAK_BEVESTIGINGSOPTIE,
	afspraakBevestigingsoptie,
})
