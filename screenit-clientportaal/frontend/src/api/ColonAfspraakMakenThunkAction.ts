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
import ScreenitBackend from "../utils/Backend"
import {ToastMessageType} from "../datatypes/toast/ToastMessage"
import {getString} from "../utils/TekstPropertyUtil"
import {ExceptieOmschrijvingDto} from "../datatypes/ExceptieOmschrijvingDto"
import HttpStatusCode from "../datatypes/HttpStatus"
import properties from "../pages/bvo/colon/afspraak/ColonAfspraakMakenBevestigingsPopup.json"
import propertiesExceptie from "../pages/bvo/colon/afspraak/ColonAfspraakMakenBevestigingsPopup.json"
import {showToast} from "../utils/ToastUtil"
import {VrijSlotZonderKamer} from "../datatypes/VrijSlotZonderKamer"
import {ColonIntakeafspraakType} from "../datatypes/colon/ColonIntakeafspraakType"

export const afspraakVerplaatsen = (nieuweAfspraak: VrijSlotZonderKamer, onError?: () => void) => () => {
	return voerActieUit("colon/afspraak/verplaatsen", nieuweAfspraak, onError)
}

export const nieuweAfspraak = (nieuweAfspraak: VrijSlotZonderKamer, onError?: () => void) => () => {
	return voerActieUit("colon/afspraak/maken", nieuweAfspraak, onError)
}

function voerActieUit(backendUrl: string, nieuweAfspraak: VrijSlotZonderKamer, onError?: () => void): Promise<void> {
	return ScreenitBackend.put(backendUrl, {json: nieuweAfspraak})
		.then(() => {
			showToast(getToastTitel(nieuweAfspraak.type), getString(nieuweAfspraak.type === ColonIntakeafspraakType.OP_LOCATIE ? properties.toast.bevestiging.message.op_locatie : properties.toast.bevestiging.message.digitaal))
		})
		.catch((error) => {
			if (error.response.status === HttpStatusCode.CONFLICT) {
				setExceptieOmschrijvingDto(error.response.data)
			}

			if (onError) {
				onError()
			}
		})
}

function getToastTitel(nieuweAfspraakType: string) {
	return nieuweAfspraakType === ColonIntakeafspraakType.OP_LOCATIE ? properties.toast.bevestiging.title.op_locatie : properties.toast.bevestiging.title.digitaal
}

function setExceptieOmschrijvingDto(data: string): void {
	const jsonOutput: ExceptieOmschrijvingDto = JSON.parse(data)

	showToast(getString(propertiesExceptie.toast.errors.verplaatsen["afspraak.niet.gemaakt"]), jsonOutput.additionalMessage, ToastMessageType.ERROR)
}
