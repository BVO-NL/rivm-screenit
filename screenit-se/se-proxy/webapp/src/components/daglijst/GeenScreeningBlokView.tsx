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
import {GeenScreeningBlok} from "../../datatypes/Planning"
import {getTime} from "../../util/DateUtil"

type GeenScreeningBlokProps = {
	geenScreeningBlok: GeenScreeningBlok;
};

export default class GeenScreeningBlokView extends Component<GeenScreeningBlokProps> {
	render(): JSX.Element {
		return <tr style={{backgroundColor: "#B9B9B9"}}>
			<td>{`${this.props.geenScreeningBlok.vanafTijd} - ${getTime(this.props.geenScreeningBlok.totDatumTijd)}`}</td>
			<td colSpan={4}>{this.props.geenScreeningBlok.opmerking}</td>
		</tr>
	}

}
