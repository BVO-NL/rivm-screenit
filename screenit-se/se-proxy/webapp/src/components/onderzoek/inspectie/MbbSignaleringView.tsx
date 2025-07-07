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
import {Card, CardBody, Row} from "reactstrap"
import DropdownValue from "../../generic/DropdownValue"
import type {SuboptimaleInsteltechniek} from "../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek"
import {getSuboptimaleInsteltechniekBeschrijving} from "../../../datatypes/visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek"
import TextAreaValue from "../../generic/TextAreaValue"
import type {RedenFotobespreking} from "../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking"
import {getRedenFotobesprekingString} from "../../../datatypes/visueleinspectie/mbbsignalering/RedenFotobespreking"
import {zelfdeTekst} from "../../../util/StringUtil"
import {getMandatory} from "../../../util/MapUtil"
import CheckboxValue from "../../generic/CheckboxValue"

export type MbbSignaleringViewStateProps = {
	afspraakId: number;
	suboptimaleInsteltechniek?: SuboptimaleInsteltechniek;
	redenFotobespreking?: RedenFotobespreking;
	extraMedewerkerId?: number;
	seGebruikers: Map<string, string>;
	ingelogdeGebruikerId?: number;
	opmerkingMbber?: string;
	opmerkingVoorRadioloog?: string;
	operatieRechts?: boolean;
	operatieLinks?: boolean;
	aanvullendeInformatieOperatie?: string;
	huidscheuring?: boolean;
	disabled: boolean;
};

export type MbbSignaleringViewDispatchProps = {
	verwerkInsteltechniek: (afspraakId: number, suboptimaleInsteltechniek?: SuboptimaleInsteltechniek) => void;
	verwerkRedenFotobespreking: (afspraakId: number, redenFotobespreking?: RedenFotobespreking) => void;
	verwerkExtraMedewerkerId: (afspraakId: number, extraMedewerkerId?: number) => void;
	verwerkOpmerkingMbber: (afspraakId: number, opmerking: string) => void;
	verwerkOpmerkingVoorRadioloog: (afspraakId: number, opmerking: string) => void;
	verwerkOperatieRechtsChanged: (afspraakId: number, operatieR: boolean) => void;
	verwerkOperatieLinksChanged: (afspraakId: number, operatieL: boolean) => void;
	verwerkAanvullendeInformatieOperatie: (afspraakId: number, value: string) => void;
	verwerkHuidscheuringChanged: (afspraakId: number, operatieR: boolean) => void;
}

const optionInsteltechniek: Array<SuboptimaleInsteltechniek> = ["FYSIEK_BEPERKT", "MOBIEL_BEPERKT", "MOEILIJK_TE_POSITIONEREN"]
const optionFotobespreking: Array<RedenFotobespreking> = ["ARCHITECTUURVERSTORING", "ASYMMETRIE", "CALCIFICATIES", "MASSA", "INSTELTECHNIEK", "OVERIG"]
const maxOpmerkingLengte = 255
export default class MbbSignaleringView extends Component<MbbSignaleringViewStateProps & MbbSignaleringViewDispatchProps> {

	constructor(props: MbbSignaleringViewStateProps & MbbSignaleringViewDispatchProps) {
		super(props)
		this.suboptimaleInsteltechniekDidChange.bind(this)
		this.redenFotoBesprekingDidChange.bind(this)
		this.extraMedewerkerDidChange.bind(this)
		this.opmerkingMbberDidChange.bind(this)
		this.opmerkingVoorRadioloogDidChange.bind(this)
		this.operatieRechtsDidChange.bind(this)
		this.operatieLinksDidChange.bind(this)
		this.aanvullendeInformatieOperatieDidChange.bind(this)
		this.zonderIngelogdeGebruikerFilterFunction.bind(this)
		this.huidscheuringDidChange.bind(this)
	}

	suboptimaleInsteltechniekDidChange = (value?: SuboptimaleInsteltechniek): void => {
		if (value !== this.props.suboptimaleInsteltechniek) {
			this.props.verwerkInsteltechniek(this.props.afspraakId, value)
		}
	}

	redenFotoBesprekingDidChange = (value?: RedenFotobespreking): void => {
		if (value !== this.props.redenFotobespreking) {
			this.props.verwerkRedenFotobespreking(this.props.afspraakId, value)
		}
	}

	extraMedewerkerDidChange = (value?: [string, string]): void => {
		if (!value) {
			this.props.verwerkExtraMedewerkerId(this.props.afspraakId, undefined)
		} else if (value[0] !== String(this.props.extraMedewerkerId)) {
			this.props.verwerkExtraMedewerkerId(this.props.afspraakId, Number(value[0]))
		}
	}

	opmerkingMbberDidChange = (value: string): void => {
		if (!zelfdeTekst(value, this.props.opmerkingMbber)) {
			this.props.verwerkOpmerkingMbber(this.props.afspraakId, value)
		}
	}

	opmerkingVoorRadioloogDidChange = (value: string): void => {
		if (!zelfdeTekst(value, this.props.opmerkingVoorRadioloog)) {
			this.props.verwerkOpmerkingVoorRadioloog(this.props.afspraakId, value)
		}
	}

	operatieRechtsDidChange = (): void => {
		this.props.verwerkOperatieRechtsChanged(this.props.afspraakId, !this.props.operatieRechts)
	}

	operatieLinksDidChange = (): void => {
		this.props.verwerkOperatieLinksChanged(this.props.afspraakId, !this.props.operatieLinks)
	}

	aanvullendeInformatieOperatieDidChange = (value: string): void => {
		if (!zelfdeTekst(value, this.props.aanvullendeInformatieOperatie)) {
			this.props.verwerkAanvullendeInformatieOperatie(this.props.afspraakId, value)
		}
	}

	huidscheuringDidChange = (): void => {
		this.props.verwerkHuidscheuringChanged(this.props.afspraakId, !this.props.huidscheuring)
	}

	zonderIngelogdeGebruikerFilterFunction = (gebruiker: [string, string]): boolean => {
		return gebruiker[0] !== String(this.props.ingelogdeGebruikerId)
	}

	render(): JSX.Element {
		return <Card className={"visuele-inspectie-row onderzoek-component"}>
			<CardBody>
				<h6>Suboptimale insteltechniek</h6>
				<DropdownValue id={"suboptimaleInsteltechniek"} value={this.props.suboptimaleInsteltechniek}
							   disabled={this.props.disabled} options={optionInsteltechniek}
							   valueToLabel={getSuboptimaleInsteltechniekBeschrijving} lavendel={true}
							   handleChange={this.suboptimaleInsteltechniekDidChange}/>

				<div className={"mbb-signalering-row"}>
					<h6>Reden fotobespreking</h6>
					<DropdownValue id={"redenFotobespreking"} value={this.props.redenFotobespreking}
								   disabled={this.props.disabled} options={optionFotobespreking}
								   valueToLabel={getRedenFotobesprekingString} lavendel={true}
								   handleChange={this.redenFotoBesprekingDidChange}/>
				</div>

				<div className={"mbb-signalering-row"}>
					<h6>Extra MBB&apos;er</h6>
					<DropdownValue id={"extraMedewerkerId"}
								   value={!this.props.extraMedewerkerId || !this.props.seGebruikers.has(String(this.props.extraMedewerkerId)) ? undefined : ["none", getMandatory(this.props.seGebruikers, String(this.props.extraMedewerkerId))]}
								   disabled={this.props.disabled}
								   options={Array.from(this.props.seGebruikers).filter(this.zonderIngelogdeGebruikerFilterFunction).sort((a, b) => String(a[1]) > String(b[1]) ? 1 : -1)}
								   valueToLabel={(seGebruiker): string => seGebruiker[1] ?? ""}
								   lavendel={true} handleChange={this.extraMedewerkerDidChange}/>
				</div>

				<div className={"mbb-signalering-row"}>
					<h6>Opmerking voor MBB&apos;er</h6>
					<TextAreaValue value={this.props.opmerkingMbber ?? ""} disabled={this.props.disabled}
								   maxLength={maxOpmerkingLengte} onChange={this.opmerkingMbberDidChange}
								   color={"lavender"}/>
				</div>

				<div className={"mbb-signalering-row"}>
					<h6>Opmerking voor radioloog</h6>
					<TextAreaValue value={this.props.opmerkingVoorRadioloog ?? ""} disabled={this.props.disabled}
								   maxLength={maxOpmerkingLengte} onChange={this.opmerkingVoorRadioloogDidChange}
								   color={"lavender"}/>
				</div>

				<div className={"mbb-signalering-row"}>
					<Row noGutters>
						<CheckboxValue
							label={"Operatie R"} checked={this.props.operatieRechts}
							disabled={this.props.disabled}
							handleChange={this.operatieRechtsDidChange}/>

						<CheckboxValue
							label={"Operatie L"} checked={this.props.operatieLinks}
							disabled={this.props.disabled}
							handleChange={this.operatieLinksDidChange}/>
					</Row>
					<TextAreaValue className={"mbb-signalering-text-area-input"}
								   value={this.props.aanvullendeInformatieOperatie ?? ""}
								   placeholder={"Aanvullende informatie"} disabled={this.props.disabled}
								   maxLength={maxOpmerkingLengte} onChange={this.aanvullendeInformatieOperatieDidChange}
								   color={"lavender"}/>
				</div>

				<div className={"mbb-signalering-row"}>
					<CheckboxValue
						label={"Huidscheuring opgetreden"} checked={this.props.huidscheuring}
						disabled={this.props.disabled}
						handleChange={this.huidscheuringDidChange}/>
				</div>
			</CardBody>
		</Card>
	}

}
