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

export type DagSynchronisatieViewStateProps = {
	daglijstDatum: string;
	dagverslag: Map<string, Dagverslag>;
};

export default class DagSynchronisatieView extends React.Component<DagSynchronisatieViewStateProps> {
	render(): JSX.Element {
		const dagverslag = this.props.dagverslag.get(this.props.daglijstDatum)

		if (!dagverslag) {
			return <div/>
		}

		return <Paneel className="dagverslag-paneel">
			<h6>
				Onderzoeken synchronisatie
			</h6>
			<Table className="table table-bordered table-condensed table-hover table-duo">
				<thead>
				<tr>
					<th>
						Onderzoeken gestart
					</th>
					<th>
						Onderzoeken met beelden verwerkt
					</th>
				</tr>
				</thead>
				<tbody>
				<tr>
					<td>
						{dagverslag.dagSynchronisatie.gemaakt}
					</td>
					<td>
						{dagverslag.dagSynchronisatie.verwerkt}
					</td>
				</tr>
				</tbody>
			</Table>
		</Paneel>
	}

}
