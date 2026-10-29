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
import {State} from "../datatypes/State"
import {VrijSlotZonderKamer} from "../datatypes/VrijSlotZonderKamer"
import {ColonDossier} from "../datatypes/colon/ColonDossier"
import {ColonHeeftAsaScoreBovenDrie} from "../datatypes/colon/ColonHeeftAsaScoreBovenDrie"
import {ColonIntakeafspraakDto} from "../datatypes/colon/ColonIntakeafspraakDto"
import {ColonIntakeafspraakType} from "../datatypes/colon/ColonIntakeafspraakType"

export const selectIntakeAfspraak = (state: State): ColonIntakeafspraakDto | undefined => state.client.colonDossier.intakeAfspraak
export const selectColonDossier = (state: State): ColonDossier => state.client.colonDossier
export const selectHeraanmelding = (state: State): boolean | undefined => {
	const huidigeIntakeAfspraak = selectIntakeAfspraak(state)
	return huidigeIntakeAfspraak?.afspraakAfgezegd || huidigeIntakeAfspraak?.andereIntakelocatieOpVerzoekClient
}
export const selectGeselecteerdSlot = (state: State): VrijSlotZonderKamer | null => state.client.colonDossier.slot
export const selectIntakeafspraakType = (state: State): ColonIntakeafspraakType | undefined => state.client.colonDossier.geselecteerdeIntakeafspraakType
export const selectHeeftAsaScoreBovenDrie = (state: State): ColonHeeftAsaScoreBovenDrie | undefined => state.client.colonDossier.heeftAsaScoreBovenDrie
export const selectAfspraakTypeDigitaal = (state: State): boolean => state.client.colonDossier.geselecteerdeIntakeafspraakType === ColonIntakeafspraakType.DIGITAAL
