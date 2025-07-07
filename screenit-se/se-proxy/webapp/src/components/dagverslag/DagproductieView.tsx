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
import React from "react"
import {Table} from "reactstrap"
import Paneel from "../generic/Paneel"
import type {Dagverslag} from "../../datatypes/Dagverslag"

export type DagproductieViewProps = {
	daglijstDatum: string;
	dagverslag: Map<string, Dagverslag>;
};

export default class DagproductieView extends React.Component<DagproductieViewProps> {
	render(): JSX.Element {
		const dagverslag = this.props.dagverslag.get(this.props.daglijstDatum)

		if (!dagverslag) {
			return <div/>
		}

		const dagproductieTabelRijen = this.getAlleMedewerkerRijen(dagverslag)
		return <Paneel className="dagverslag-paneel">
			<h6>
				Dagproductie
			</h6>
			<Table className="table table-bordered table-condensed table-hover">
				<thead>
				<tr>
					<th>
						Medewerker
					</th>
					<th>
						Ingeschreven
					</th>
					<th>
						Onderzocht
					</th>
					<th>
						Afgerond
					</th>
					<th>
						Onderbroken
					</th>
					<th>
						Onvolledig
					</th>
					<th>
						Afwijkingen
					</th>
				</tr>
				</thead>
				<tbody>
				{dagproductieTabelRijen}
				</tbody>
			</Table>
		</Paneel>
	}

	getAlleMedewerkerRijen = (dagverslag?: Dagverslag): JSX.Element[] => {
		const result: JSX.Element[] = []
		if (!dagverslag) {
			return result
		}

		const dagproductie = dagverslag.dagproductie
		for (const medewerker in dagproductie) {
			result.push(<tr key={`${medewerker}-dagproductie`}>
				<td>
					{medewerker}
				</td>
				<td>
					{dagproductie[medewerker]?.ingeschrevenCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onderzochtCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.afgerondCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onderbrokenCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.onvolledigCount || 0}
				</td>
				<td>
					{dagproductie[medewerker]?.afwijkingenCount || 0}
				</td>
			</tr>)
		}

		return result
	}
}
