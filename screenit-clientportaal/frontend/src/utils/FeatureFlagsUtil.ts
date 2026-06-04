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
import {PreferenceKey} from "../datatypes/PreferenceKey"
import {cpStore} from "../store"
import {Parameter} from "../datatypes/Parameter"
import {isBefore, isValid, parse} from "date-fns"

export function isDigitaleIntakeBeschikbaar(): boolean {
	const parameter: Parameter = cpStore.getState().parameters.find((parameter: Parameter) => parameter.naam === PreferenceKey.COLON_START_DIGITALE_INTAKE)
	if (!parameter?.waarde) {
		return false
	}
	const startDatum = parse(parameter.waarde as string, "yyyyMMdd", new Date())
	return isValid(startDatum) && isBefore(startDatum, new Date())
}
