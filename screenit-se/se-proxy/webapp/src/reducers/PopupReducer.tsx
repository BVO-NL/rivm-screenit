/*-
 * ========================LICENSE_START=================================
 * se-proxy
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
import type {PopupActions} from "../actions/PopupActions"
import {AKKOORD_POPUP, CLEAR_POPUP, SHOW_POPUP} from "../actions/PopupActions"
import type {Popup} from "../datatypes/Popup"
import {CLEAR_CACHE, ClearCacheActions} from "../actions/ClearCacheActions"
import {Reducer} from "redux"

const emptyPopup: Popup = {
	titel: "",
	body: undefined,
	visible: false,
	annulerenString: "Annuleren",
	akkoordString: "Akkoord",
	callback: undefined,
	cancelCallback: undefined,
	alleenOnline: false,
}

const PopupReducer: Reducer<Popup, PopupActions | ClearCacheActions> = (stateSlice = emptyPopup, action) => {
	switch (action.type) {
		case SHOW_POPUP:
			return {
				titel: action.titel,
				body: action.body,
				visible: true,
				callback: action.callback,
				cancelCallback: action.cancelCallback,
				akkoordString: action.akkoordString,
				annulerenString: action.annulerenString,
				alleenOnline: action.alleenOnline,
			}
		case AKKOORD_POPUP:
			return {
				...stateSlice,
				...{
					visible: false,
				},
			}
		case CLEAR_POPUP:
			return {
				...stateSlice,
				...{
					visible: false,
				},
			}
		case CLEAR_CACHE:
			return emptyPopup
		default:
			return stateSlice
	}
}

export default PopupReducer
