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
import React, {Component} from "react"
import {Button} from "reactstrap"
import {store} from "../../Store"
import {createActionSetHuidigeMammograaf} from "../../actions/MammograafActions"

export default class WerkstationZonderMammograafknopView extends Component<unknown> {

	constructor(props: unknown) {
		super(props)
		this.onClick = this.onClick.bind(this)
	}

	render(): JSX.Element {
		return <div>
			<Button className="btn-primary-se align-middle" onClick={this.onClick}>Werkstation zonder mammograaf</Button>
		</div>
	}

	onClick = (): void => {
		store.dispatch(createActionSetHuidigeMammograaf(0))
	}
}
