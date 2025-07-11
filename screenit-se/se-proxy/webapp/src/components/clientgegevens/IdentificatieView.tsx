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
import {Col, Label, Row} from "reactstrap"
import type {Afspraak, Identificatiesoort} from "../../datatypes/Afspraak"
import {alleIdentificatieSoorten, getIdentificatieMaxLength, identificatiesoortNaam} from "../../datatypes/Afspraak"
import PaneelNaam from "../generic/PaneelNaam"
import Paneel from "../generic/Paneel"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import ValidationInputContainer from "../generic/ValidationInputContainer"
import DropdownValue from "../generic/DropdownValue"

export type IdentificatieViewStateProps = {
	afspraak: Afspraak;
	clientGegevensForm: Form;
	disabled: boolean;
};

export type IdentificatieViewDispatchProps = {
	onChooseSoort: (soort: Identificatiesoort, form: Form) => void;
	onChooseNummer: (nummer: string) => void;
	updateField: <T>(value: T | string, fieldId: FORM_FIELD_ID, form: Form, showError: boolean | undefined) => void;
}

type IdentificatieState = {
	dropdownOpen: boolean;
};

export const IDENTIFICATIE_FIELD_ID: FORM_FIELD_ID = {
	formId: "clientgegevens",
	fieldId: "identificatienummer",
}

export default class IdentificatieView extends Component<IdentificatieViewStateProps & IdentificatieViewDispatchProps, IdentificatieState> {

	constructor(props: IdentificatieViewStateProps & IdentificatieViewDispatchProps) {
		super(props)
		this.toggle = this.toggle.bind(this)
		this.onIdentificatieSoortChange = this.onIdentificatieSoortChange.bind(this)
		this.state = {
			dropdownOpen: false,
		}
	}

	toggle = (): void => {
		this.setState({
			dropdownOpen: !this.state.dropdownOpen,
		})
	}

	onIdentificatieSoortChange = (value: Identificatiesoort | undefined): void => {
		if (value) {
			this.props.onChooseSoort(value, this.props.clientGegevensForm)
		}
	}

	onIdentificatieNummerChange = (value: string): void => {
		if (this.props.afspraak.identificatiesoort !== "OVERIG") {
			value = value.toUpperCase()
		}

		this.props.onChooseNummer(value)
	}

	render(): JSX.Element {
		return <Paneel>
			<PaneelNaam titel={"Identificatie*"}/>
			<Row noGutters>
				<Col md={2} className={"identificatie-soort-column"}>
					<DropdownValue id={"identificatiesoort"} value={this.props.afspraak.identificatiesoort}
								   disabled={this.props.disabled} required={true} options={alleIdentificatieSoorten}
								   valueToLabel={identificatiesoortNaam} handleChange={this.onIdentificatieSoortChange}
								   isWhite={true} placeholder={"Kies identificatie..."}/>
				</Col>
				<Col md={3}>
					<Label className="sr-only" for="identificatienummerInput">Identificatienummer</Label>
					<ValidationInputContainer
						id="identificatienummerInput" disabled={this.props.disabled}
						fieldId={IDENTIFICATIE_FIELD_ID}
						maxLength={getIdentificatieMaxLength(this.props.afspraak.identificatiesoort)}
						value={!this.props.afspraak.identificatienummer ? "" : this.props.afspraak.identificatienummer}
						transformValue={(value): any => {
							return {
								identificatiesoort: this.props.afspraak.identificatiesoort,
								identificatienummer: value,
							}
						}} onChange={this.onIdentificatieNummerChange}
						placeholder="Voer identificatienummer in"/>
				</Col>
			</Row>

		</Paneel>
	}

}
