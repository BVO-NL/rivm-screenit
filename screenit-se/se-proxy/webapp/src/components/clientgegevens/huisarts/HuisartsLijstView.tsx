/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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
import {Component, JSX} from "react"
import type {Huisarts} from "../../../datatypes/Huisarts"
import {Afspraak} from "../../../datatypes/Afspraak"
import {getMandatory} from "../../../util/MapUtil"
import type {HuisartsZoekFilter} from "./HuisartsZoekenView"
import DataTable from "react-data-table-component"

export type HuisartsListItem = {
	id: number;
	naam: string;
	type: string;
	adres: string;
	praktijknaam: string;
};

export type HuisartsLijstStateProps = {
	huisartsen: Map<number, Huisarts>;
	huisartsFilter: HuisartsZoekFilter;
	huisartsItems: Array<HuisartsListItem>;
	afspraak: Afspraak;
	moetVerversen: boolean;
};

export type HuisartsLijstDispatchProps = {
	vergrendelZoekState: () => void;
	onKiesHuisarts: (huisarts: Huisarts) => void;
	toggle: () => void;
}

export type HuisartsLijstState = {
	moetVerversen: boolean;
};

const huisartsTableColumns = [{
	selector: (row: HuisartsListItem): string => row.naam,
	name: "Naam",
	sortable: true,
}, {
	selector: (row: HuisartsListItem): string => row.type,
	name: "Type",
	sortable: true,
	width: "200px",
}, {
	selector: (row: HuisartsListItem): string => row.adres,
	name: "Adres",
	sortable: true,
}, {
	selector: (row: HuisartsListItem): string => row.praktijknaam,
	name: "Praktijknaam",
	sortable: true,
}]

export default class HuisartsLijstView extends Component<HuisartsLijstStateProps & HuisartsLijstDispatchProps> {

	onSelect(row: HuisartsListItem): void {
		this.props.onKiesHuisarts(getMandatory(this.props.huisartsen, row.id))
		this.props.toggle()
	}

	render(): JSX.Element {
		return <div className={"huisarts-table"}>
			<h5 className={"ml-2"}>Er zijn {this.props.huisartsItems.length} huisartsen gevonden</h5>
			<DataTable data={this.props.huisartsItems} columns={huisartsTableColumns} pagination onRowClicked={row => this.onSelect(row)}
					   paginationPerPage={5} paginationComponentOptions={{noRowsPerPage: true, rangeSeparatorText: "van"}} pointerOnHover={true}/>
		</div>
	}

	shouldComponentUpdate(props: HuisartsLijstStateProps & HuisartsLijstDispatchProps): boolean {
		this.props.vergrendelZoekState()
		return props.moetVerversen
	}

}
