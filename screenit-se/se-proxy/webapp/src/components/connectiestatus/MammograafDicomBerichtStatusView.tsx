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
import React, {Component} from "react"
import type {ConnectieStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"
import {getTijdGeledenTekst} from "../../util/DateUtil"
import {MammograafDicomMessageType} from "../../datatypes/connectiestatus/MammograafDicomMessageError"

export type MammograafDicomBerichtStatusStateProps = {
	messageType: MammograafDicomMessageType;
	level: ConnectieStatusLevel;
	timestamp?: string;
};

export type MammograafDicomBerichtStatusDispatchProps = {
	onClick: () => void;
};

export default class MammograafDicomBerichtStatusView extends Component<MammograafDicomBerichtStatusStateProps & MammograafDicomBerichtStatusDispatchProps> {
	render(): JSX.Element {
		switch (this.props.level) {
			case "FAULT":
				return <p><span>Er zijn <u onClick={this.props.onClick}><b>fouten</b></u> opgetreden </span>
					{this.props.timestamp ?
						<span>na het laatst succesvolle {this.props.messageType} bericht van: <b>{getTijdGeledenTekst(this.props.timestamp)}</b></span> :
						<span>bij het eerste {this.props.messageType} bericht sinds het opstarten</span>}</p>

			case "OK":
				return <p>Laatst ontvangen
					succesvolle {this.props.messageType} bericht: <b>{this.props.timestamp ? getTijdGeledenTekst(this.props.timestamp) : "Onbekend"}</b>
				</p>

			case "WARN":
				return <p>Er is nog geen succesvol {this.props.messageType} bericht ontvangen</p>
		}
	}

}
