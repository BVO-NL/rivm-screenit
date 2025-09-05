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
import React from "react"
import {Button, Col, Row} from "reactstrap"
import {Mammograaf} from "../../datatypes/Mammograaf"
import {gaOffline, gaOnline} from "../../restclient/TestRestclient"

export type HeaderViewStateProps = {
	aangemeld: boolean;
	gebruikersnaam?: string;
	displayName?: string;
	seCode?: string;
	seNaam?: string;
	huidigeMammograaf?: Mammograaf;
	online: boolean;
	isTestOmgeving: boolean;
};

export type HeaderViewDispatchProps = {
	afmelden: () => void;
}

export default class HeaderView extends React.Component<HeaderViewStateProps & HeaderViewDispatchProps> {
	render(): JSX.Element | null {
		return this.props.aangemeld ? <Col md={10} className={"float-right medewerker-view"}>
			{!this.props.online ?
				<img className="geen-verbinding" src="images/geen-verbinding.png" alt="geen-verbinding"/> : null}
			{this.props.isTestOmgeving && (this.props.online ? <div className="col-2 offline-knop">
				<input className="btn btn-danger" type="submit" value="Ga offline" onClick={(): void => {
					gaOffline()
				}}/>
			</div> : <div className="col-2 offline-knop">
				<input className="btn btn-success" type="submit" value="Ga online" onClick={(): void => {
					gaOnline()
				}}/>
			</div>)}
			<Row>
				<Col md={5}>
					<div className={"float-right"}>
						{this.props.displayName}
					</div>
				</Col>
				<Col md={3}>{this.props.seNaam}</Col>
				<Col md={2}>{this.props.huidigeMammograaf ? this.props.huidigeMammograaf.aeTitle : ""}</Col>
				<Col md={2}>
					<Button
						id="afmeldButton" color={"link"} className={"btn mr-1 white-link-button btn-sm"}
						onClick={(): void => {
							this.props.afmelden()
						}}> Afmelden </Button>
				</Col>
			</Row>
		</Col> : this.props.isTestOmgeving ? (this.props.online ?
			<Col md={10} className={"float-right medewerker-view"}>
				<div className="col-2 offline-knop">
					<input className="btn btn-danger" type="submit" value="Ga offline" onClick={(): void => {
						gaOffline()
					}}/>
				</div>
			</Col> : <Col md={10} className={"float-right medewerker-view"}>
				<div className="col-2 offline-knop">
					<input className="btn btn-success" type="submit" value="Ga online" onClick={(): void => {
						gaOnline()
					}}/>
				</div>
			</Col>) : null
	}

}
