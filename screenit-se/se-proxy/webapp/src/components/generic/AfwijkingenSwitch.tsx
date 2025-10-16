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
import Switch from "react-switch"
import {dispatchActions} from "../../util/DispatchUtil"
import {createActionUpdateHeeftAfwijkingen} from "../../actions/SignalerenActions"
import {store} from "../../Store"

type AfwijkingenSwitchProps = {
	id?: string;
	heeftRecht: boolean;
	heeftAfwijkingen: boolean;
	rechtNaam: string;
	placementFeedback?: string;
	color?: string;
	center?: boolean;
	afspraakId: number;
};

type AfwijkingenSwitchState = {
	popoverOpen: boolean;
};

export default class AfwijkingenSwitch extends Component<AfwijkingenSwitchProps, AfwijkingenSwitchState> {

	constructor(props: AfwijkingenSwitchProps) {
		super(props)
		this.toggle = this.toggle.bind(this)
		this.state = {
			popoverOpen: false,
		}
	}

	handleChange = (checked: boolean): void => {
		if (this.props.heeftRecht) {
			dispatchActions(store.dispatch, createActionUpdateHeeftAfwijkingen(this.props.afspraakId, checked))
		}
	}

	toggle = (): void => {
		this.setState({
			popoverOpen: !this.state.popoverOpen,
		})
	}

	render(): JSX.Element {
		return <div className={"gutters"}>
			{this.props.heeftAfwijkingen ? <h5 className={"text-center text-shadow"}>Afwijkingen</h5> :
				<h5 className={"text-center text-shadow"}>Geen afwijkingen</h5>}
			<label htmlFor="normal-switch" className={"text-center"}>
				<Switch onChange={this.handleChange}
						checked={this.props.heeftAfwijkingen} id={"afwijkingen-switch"} width={140} height={30}
						onColor={"#E74C3C"} offColor={"#28a745"} checkedIcon={false} uncheckedIcon={false}
						boxShadow={"0 1px 3px rgba(0, 0, 0, 0.12), 0 1px 2px rgba(0, 0, 0, 0.24)"}/>
			</label>
		</div>
	}

}
