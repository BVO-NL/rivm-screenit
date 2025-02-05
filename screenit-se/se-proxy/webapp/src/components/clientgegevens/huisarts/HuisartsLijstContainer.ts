/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import type {HuisartsLijstStateProps, HuisartsListItem} from "./HuisartsLijstView"
import HuisartsLijstView from "../huisarts/HuisartsLijstView"
import {connect} from "react-redux"
import type {Huisarts} from "../../../datatypes/Huisarts"
import {getHuisartsVolledigAdres} from "../../../datatypes/Huisarts"
import {RootState} from "../../../Store"
import {HuisartsZoekFilter} from "./HuisartsZoekenView"
import {Afspraak} from "../../../datatypes/Afspraak"

export type HuisartsLijstContainerProps = {
	huisartsFilter: HuisartsZoekFilter;
	afspraak: Afspraak;
	vergrendelZoekState: () => void;
	onKiesHuisarts: (huisarts: Huisarts) => void;
	toggle: () => void;
	moetVerversen: boolean;
}

const mapStateToProps = (state: RootState, ownProps: HuisartsLijstContainerProps): HuisartsLijstStateProps => {
	const huisartsItems: Array<HuisartsListItem> = []
	let huisartsenFiltered: Array<Huisarts> = Array.from(state.huisartsenById.values())

	if (ownProps.huisartsFilter.naamHuisarts !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.naamHuisarts && h.naamHuisarts.toLocaleLowerCase().includes(ownProps.huisartsFilter.naamHuisarts.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.straatnaam !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.straatnaam && h.straatnaam.toLocaleLowerCase().includes(ownProps.huisartsFilter.straatnaam.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.postcode !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.postcode && h.postcode.toLocaleLowerCase().includes(ownProps.huisartsFilter.postcode.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.plaats !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.plaats && h.plaats.toLocaleLowerCase().includes(ownProps.huisartsFilter.plaats.toLocaleLowerCase()))
	}

	huisartsenFiltered.map(h => huisartsItems.push({
		id: h.id,
		naam: h.naamHuisarts,
		type: h.type,
		adres: getHuisartsVolledigAdres(h),
		praktijknaam: h.praktijknaam,
	}))
	return {
		...ownProps,
		huisartsItems: huisartsItems,
		huisartsen: state.huisartsenById,
	}
}

const HuisartsLijstContainer = connect(mapStateToProps)(HuisartsLijstView)
export default HuisartsLijstContainer
