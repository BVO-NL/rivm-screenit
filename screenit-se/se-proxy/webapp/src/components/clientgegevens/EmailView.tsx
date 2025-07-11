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
import PaneelNaam from "../generic/PaneelNaam"
import Paneel from "../generic/Paneel"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import ValidationInputContainer from "../generic/ValidationInputContainer"
import {Col, Row} from "reactstrap"

export type EmailViewStateProps = {
	clientId: number;
	emailadres?: string;
	disabled: boolean;
	clientGegevensForm: Form;
};

export type EmailViewDispatchProps = {
	setEmailAdres: (clientId: number, emailadres: string) => void;
}

export const EMAIL_FIELD_ID: FORM_FIELD_ID = {
	formId: "clientgegevens",
	fieldId: "email",
}

export default class EmailView extends Component<EmailViewStateProps & EmailViewDispatchProps> {
	constructor(props: EmailViewStateProps & EmailViewDispatchProps) {
		super(props)
		this.onEmailChange = this.onEmailChange.bind(this)
	}

	onEmailChange = (value: string): void => {
		if (value !== this.props.emailadres) {
			this.props.setEmailAdres(this.props.clientId, value)
		}
	}

	render(): JSX.Element {
		return <Paneel>
			<PaneelNaam titel={"E-mailadres"}/>
			<div className={"form-row"}>
				<Col>
					<Row noGutters>
						<Col md={2}>
							E-mailadres
						</Col>
						<Col md={3}>
							<ValidationInputContainer
								label={"E-mailadres"} placeholder={"Voer e-mailadres in"}
								disabled={this.props.disabled} value={this.props.emailadres || ""}
								onChange={this.onEmailChange} fieldId={EMAIL_FIELD_ID}
								maxLength={100}/>
						</Col>
					</Row>
				</Col>
			</div>
		</Paneel>
	}

}
