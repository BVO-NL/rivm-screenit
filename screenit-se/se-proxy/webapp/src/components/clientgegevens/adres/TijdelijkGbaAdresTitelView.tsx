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
import React, {Component} from "react"
import {Col, Row, UncontrolledTooltip} from "reactstrap"

export default class TijdelijkGbaAdresTitelView extends Component<Record<string, any>> {
	render(): JSX.Element {
		return <Col>
			<Row>
				<Col className="paneelnaam">
					Tijdelijk GBA adres
					<i id="AdresTooltip" className="icon-spacing fa fa-info-circle" aria-hidden="true"/>
					<UncontrolledTooltip placement="right" target="AdresTooltip">
						Dit is het correspondentie adres zolang er geen volledig GBA adres is.
					</UncontrolledTooltip>
				</Col>
			</Row>
		</Col>
	}

}
