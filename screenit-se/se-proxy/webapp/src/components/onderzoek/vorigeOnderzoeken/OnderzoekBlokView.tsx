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
import {Col, Row, Table} from "reactstrap"
import Paneel from "../../generic/Paneel"
import LabelValue from "../../generic/LabelValue"
import {datumFormaat} from "../../../util/DateUtil"
import type {VorigOnderzoek} from "../../../datatypes/VorigOnderzoek"
import VisueleInspectieContainerView from "../inspectie/VisueleInspectieContainerView"
import AfwijkingenContainerView from "../signaleren/AfwijkingenContainerView"
import type {DoorsnedeAfbeeldingen} from "../../../datatypes/DoorsnedeAfbeeldingen"

export type OnderzoekBlokViewProps = {
	vorigOnderzoek: VorigOnderzoek;
};

export default class OnderzoekBlokView extends Component<OnderzoekBlokViewProps> {

	emptyLezingenAanzichten: DoorsnedeAfbeeldingen

	constructor(props: OnderzoekBlokViewProps) {
		super(props)
		this.emptyLezingenAanzichten = {
			rechtsVerticaleDoorsnede: undefined,
			linksVerticaleDoorsnede: undefined,
			rechtsHorizontaleDoorsnede: undefined,
			linksHorizontaleDoorsnede: undefined,
		}
	}

	render(): JSX.Element {
		try {
			return <Paneel className={"onderzoek-component paneel-shadow"}>
				<Row noGutters>
					<Col md={6} className={"px-2"}>
						<LabelValue label={"Datum onderzoek:"} value={datumFormaat(this.props.vorigOnderzoek.onderzoekDatum)}/>
					</Col>
					<Col md={6} className={"px-2"}>
						<LabelValue label={"MBB'er:"} value={this.props.vorigOnderzoek.uitvoerendMbber}/>
					</Col>
				</Row>
				<hr/>
				<Row noGutters>
					<Col md={3} className={"px-2"}>
						<VisueleInspectieContainerView afspraakId={0}
													   containerId={this.props.vorigOnderzoek.uitnodigingsNr.toString()}
													   iconenById={this.props.vorigOnderzoek.visueleInspectieAfbeelding ? this.props.vorigOnderzoek.visueleInspectieAfbeelding.iconenById : new Map()}
													   paletIconen={[]} noGutters={true} isEditable={false}
													   amputatie={this.props.vorigOnderzoek.onderzoek.amputatie}/>
					</Col>
					<Col className={"px-2"} md={3}>
						<AfwijkingenContainerView blokId={String(this.props.vorigOnderzoek.uitnodigingsNr)}
												  lezingAanzichten={this.props.vorigOnderzoek.signaleren && this.props.vorigOnderzoek.signaleren.doorsnedeAfbeeldingen ? this.props.vorigOnderzoek.signaleren.doorsnedeAfbeeldingen : this.emptyLezingenAanzichten}
												  afspraakId={0} isEditable={false}
												  amputatie={this.props.vorigOnderzoek.onderzoek.amputatie}/>
					</Col>

					<Col md={6} className={"px-2"}>
						<Table striped>
							<tbody>
							{this.props.vorigOnderzoek.teksten.map((row, index) => this.maakOnderzoekRow(row.key, row.value, index))}
							</tbody>
						</Table>
					</Col>
				</Row>
			</Paneel>
		} catch (exception: any) {
			console.warn(`fout tijdens aanmaken van onderzoek blok view: ${exception.message}`)
			return <Paneel className={"onderzoek-component paneel-shadow"}>Door een technische fout kan de vorige
				onderzoeksinformatie (gedeeltelijk) niet weergegeven worden. U kunt
				het onderzoek normaal verder uitvoeren</Paneel>
		}
	}

	maakOnderzoekRow = (label: string, property: string, index: number): React.ReactNode => {
		return <tr key={index}>
			<td>{label}</td>
			<th scope="row">{property}</th>
		</tr>
	}
}
