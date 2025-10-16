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
import Paneel from "../generic/Paneel"
import PaneelNaam from "../generic/PaneelNaam"
import CheckboxValue from "../generic/CheckboxValue"

export type BezwaarStateProps = {
	afspraakId: number;
	bezwaarAangevraagd: boolean;
	isIngeschreven: boolean;
	bezwaarDoorgevoerdOpCentraal: boolean;
};

export type BezwaarDispatchProps = {
	onBezwaarAangevraagd: (afspraakId: number, bezwaar: boolean, isIngeschreven: boolean) => void;
}

export default class BezwaarView extends Component<BezwaarStateProps & BezwaarDispatchProps> {

	constructor(props: BezwaarStateProps & BezwaarDispatchProps) {
		super(props)
		this.bezwaarAanvragenDidChange.bind(this)
	}

	bezwaarAanvragenDidChange = (value: boolean): void => {
		this.props.onBezwaarAangevraagd(this.props.afspraakId, value, this.props.isIngeschreven)
	}

	render(): JSX.Element {
		return <Paneel>
			<PaneelNaam titel={"Keuze gebruik gegevens"}/>
			<CheckboxValue label={"Brief aanvraag gebruik gegevens"}
						   checked={this.props.bezwaarAangevraagd}
						   handleChange={this.bezwaarAanvragenDidChange}
						   disabled={this.props.bezwaarDoorgevoerdOpCentraal}/>
		</Paneel>
	}

}
