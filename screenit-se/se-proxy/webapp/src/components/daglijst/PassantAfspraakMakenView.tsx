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
import {Col, Form as BootstrapForm, FormGroup, Label, Row} from "reactstrap"
import AutorisatieButton from "../generic/AutorisatieButton"
import ValidationInputContainer from "../generic/ValidationInputContainer"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import {getIfExists, getMandatory} from "../../util/MapUtil"

export type PassantAfspraakMakenViewStateProps = {
	heeftInschrijvenRecht: boolean;
	passantAfspraakMakenForm: Form;
	online: boolean;
	datumNietVandaag: boolean;
};

export type PassantAfspraakMakenViewDispatchProps = {
	onInitializeForm: () => void;
	maakAfspraak: (form: Form) => void;
}

export const GEBOORTEDATUM_FIELD_ID: FORM_FIELD_ID = {
	formId: "passant_afspraak_maken",
	fieldId: "geboortedatum",
}

export const BSN_FIELD_ID: FORM_FIELD_ID = {
	formId: "passant_afspraak_maken",
	fieldId: "bsn",
}

export default class PassantAfspraakMakenView extends Component<PassantAfspraakMakenViewStateProps & PassantAfspraakMakenViewDispatchProps> {

	constructor(props: PassantAfspraakMakenViewStateProps & PassantAfspraakMakenViewDispatchProps) {
		super(props)
		this.props.onInitializeForm()
	}

	render(): JSX.Element {
		if (this.props.datumNietVandaag) {
			return <div/>
		}

		return <div className="afspraaklijst">

			<BootstrapForm inline onSubmit={(e): void => {
				e.preventDefault()
			}}>
				<Row noGutters>
					<Col md={5}>
						<FormGroup className="mb-2 mr-sm-2 mb-sm-0">
							<Row noGutters>
								<Col md={4}>
									<div className="mr-sm-2">Geboortedatum</div>
								</Col>
								<Col md={6}>
									<ValidationInputContainer id={"geboortedatum"} type={"date"} label={"Geboortedatum"}
															  placeholder={"Voer geboortedatum in"}
															  disabled={!this.props.heeftInschrijvenRecht}
															  className={"unstyled"} value={this.getGeboortedatum()}
															  onChange={undefined} fieldId={GEBOORTEDATUM_FIELD_ID} maxLength={10}/>
								</Col>
							</Row>
						</FormGroup>
					</Col>
					<Col md={5}>
						<FormGroup className="mb-2 mr-sm-2 mb-sm-0">
							<Row>
								<Col md={4}>
									<Label>Burgerservicenummer</Label>
								</Col>
								<Col md={7}>
									<ValidationInputContainer id={"bsn"} label={"BSN"} className={"unstyled"}
															  placeholder={"Voer BSN in"}
															  disabled={!this.props.heeftInschrijvenRecht}
															  value={this.getBsn()} onChange={undefined} fieldId={BSN_FIELD_ID} maxLength={9}/>
								</Col>
							</Row>
						</FormGroup>
					</Col>
					<Col md={2}>
						<AutorisatieButton id="afspraakButton" label={"Toon afspraak"} online={this.props.online}
										   heeftRecht={this.props.heeftInschrijvenRecht} rechtNaam={"inschrijven"}
										   onClick={(): void => {
											   this.props.maakAfspraak(this.props.passantAfspraakMakenForm)
										   }}/>
					</Col>
				</Row>
			</BootstrapForm>
		</div>
	}

	getGeboortedatum(): string {
		return getIfExists(this.props.passantAfspraakMakenForm.fieldsById, GEBOORTEDATUM_FIELD_ID) ? getMandatory(this.props.passantAfspraakMakenForm.fieldsById, GEBOORTEDATUM_FIELD_ID).value : ""
	}

	getBsn(): string {
		return getIfExists(this.props.passantAfspraakMakenForm.fieldsById, BSN_FIELD_ID) ? getMandatory(this.props.passantAfspraakMakenForm.fieldsById, BSN_FIELD_ID).value : ""
	}

}
