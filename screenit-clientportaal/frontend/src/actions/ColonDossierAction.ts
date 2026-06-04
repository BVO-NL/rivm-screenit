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
import {ColonDossier} from "../datatypes/colon/ColonDossier"
import {Huisarts} from "../datatypes/Huisarts"
import {FitStatus} from "../datatypes/colon/FitStatus"
import {ColonIntakeafspraakDto} from "../datatypes/colon/ColonIntakeafspraakDto"
import {HeraanmeldenOptiesDto} from "../datatypes/afmelden/HeraanmeldenOptiesDto"
import {VrijSlotZonderKamer} from "../datatypes/VrijSlotZonderKamer"
import {ColonIntakeafspraakType} from "../datatypes/colon/ColonIntakeafspraakType"
import {ColonHeeftAsaScoreBovenDrie} from "../datatypes/colon/ColonHeeftAsaScoreBovenDrie"

export type ColonDossierActions =
	ColonDossierAction
	| SetColonHuisartsVorigeRonde
	| SetColonHuisartsHuidigeRonde
	| ResetFitStatus
	| ColonIntakeafspraakAction
	| ColonIntakeafspraakTypeAction
	| ColonHeeftAsaScoreBovenDrieAction
	| ResetHeraanmeldenOpties
	| ColonVrijSlotZonderKamerAction

export const SET_COLON_DOSSIER = "SET_COLON_DOSSIER"
export type ColonDossierAction = { type: typeof SET_COLON_DOSSIER, dossier: ColonDossier }
export const createColonDossierAction = (dossier: ColonDossier): ColonDossierAction => ({
	type: SET_COLON_DOSSIER,
	dossier,
})

export type SetColonHuisartsVorigeRonde = { type: typeof SET_COLON_HUISARTS_VORIGE_RONDE, huisarts: Huisarts | undefined }
export const SET_COLON_HUISARTS_VORIGE_RONDE = "SET_COLON_HUISARTS_VORIGE_RONDE"
export const setColonHuisartsVorigeRondeReduxAction = (huisarts: Huisarts | undefined): SetColonHuisartsVorigeRonde => ({
	type: SET_COLON_HUISARTS_VORIGE_RONDE,
	huisarts,
})

export const SET_COLON_HUISARTS_HUIDIGE_RONDE = "SET_COLON_HUISARTS_HUIDIGE_RONDE"
export type SetColonHuisartsHuidigeRonde = { type: typeof SET_COLON_HUISARTS_HUIDIGE_RONDE, huisarts: Huisarts | undefined }
export const setColonHuisartsHuidigeRondeReduxAction = (huisarts: Huisarts | undefined): SetColonHuisartsHuidigeRonde => ({
	type: SET_COLON_HUISARTS_HUIDIGE_RONDE,
	huisarts,
})

export const RESET_HUIDIGE_FIT_STATUS = "RESET_HUIDIGE_FIT_STATUS"
export type ResetFitStatus = { type: typeof RESET_HUIDIGE_FIT_STATUS, fitStatus: FitStatus }
export const setHuidigeFitStatusAction = (fitStatus: FitStatus): ResetFitStatus => ({
	type: RESET_HUIDIGE_FIT_STATUS,
	fitStatus,
})

export const HUIDIGE_INTAKE_AFSPRAAK = "SET_HUIDIGE_INTAKE_AFSPRAAK"
export type ColonIntakeafspraakAction = { type: typeof HUIDIGE_INTAKE_AFSPRAAK, colonIntakeafspraakDto: ColonIntakeafspraakDto }
export const setColonIntakeafspraakAction = (colonIntakeafspraakDto: ColonIntakeafspraakDto): ColonIntakeafspraakAction => ({
	type: HUIDIGE_INTAKE_AFSPRAAK,
	colonIntakeafspraakDto: colonIntakeafspraakDto,
})

export const COLON_INTAKEAFSPRAAK_TYPE = "SET_COLON_INTAKEAFSPRAAK_TYPE"
export type ColonIntakeafspraakTypeAction = { type: typeof COLON_INTAKEAFSPRAAK_TYPE, intakeafspraakType: ColonIntakeafspraakType }
export const setColonIntakeafspraakTypeAction = (intakeafspraakType: ColonIntakeafspraakType): ColonIntakeafspraakTypeAction => ({
	type: COLON_INTAKEAFSPRAAK_TYPE,
	intakeafspraakType,
})

export const COLON_HEEFT_ASA_SCORE_BOVEN_DRIE = "SET_COLON_HEEFT_ASA_SCORE_BOVEN_DRIE"
export type ColonHeeftAsaScoreBovenDrieAction = { type: typeof COLON_HEEFT_ASA_SCORE_BOVEN_DRIE, heeftAsaScoreBovenDrie: ColonHeeftAsaScoreBovenDrie }
export const setColonHeeftAsaScoreBovenDrieAction = (heeftAsaScoreBovenDrie: ColonHeeftAsaScoreBovenDrie): ColonHeeftAsaScoreBovenDrieAction => ({
	type: COLON_HEEFT_ASA_SCORE_BOVEN_DRIE,
	heeftAsaScoreBovenDrie,
})

export const RESET_HERAANMELDENOPTIES = "RESET_HERAANMELDENOPTIES"
export type ResetHeraanmeldenOpties = { type: typeof RESET_HERAANMELDENOPTIES, heraanmeldenOpties: HeraanmeldenOptiesDto }
export const setHeraanmeldenOpties = (heraanmeldenOpties: HeraanmeldenOptiesDto): ResetHeraanmeldenOpties => ({
	type: RESET_HERAANMELDENOPTIES,
	heraanmeldenOpties,
})

export const VRIJ_SLOT_ZONDER_KAMER = "SET_VRIJ_SLOT_ZONDER_KAMER"
export type ColonVrijSlotZonderKamerAction = { type: typeof VRIJ_SLOT_ZONDER_KAMER, slot: VrijSlotZonderKamer }
export const setColonVrijSlotZonderKamerAction = (slot: VrijSlotZonderKamer): ColonVrijSlotZonderKamerAction => ({
	type: VRIJ_SLOT_ZONDER_KAMER,
	slot,
})
