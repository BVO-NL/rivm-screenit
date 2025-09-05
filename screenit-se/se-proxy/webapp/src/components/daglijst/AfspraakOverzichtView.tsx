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
import type {Tijdslot} from "../../datatypes/Planning"
import {Afspraak} from "../../datatypes/Afspraak"
import {Col, Row} from "reactstrap"
import {store} from "../../Store"

import {GEFORCEERD_DAGLIJST_OPHALEN, vernieuwAfsprakenDaglijst} from "../../restclient/DaglijstRestclient"
import {vandaagISO} from "../../util/DateUtil"
import {Client} from "../../datatypes/Client"
import {navigateToClientgegevens, navigateToOnderzoek} from "../../util/NavigationUtil"
import BarcodeReader from "../barcodereader/BarcodeReader"
import AfspraakLijstView from "./AfspraakLijstView"

export type AfspraakOverzichtViewProps = {
	nietAfgerondeTijdSlots: Array<Tijdslot>;
	afgerondeTijdSlots: Array<Tijdslot>;
	clienten: Map<number, Client>;
	daglijstDatum: string;
};

export default class AfspraakOverzichtView extends Component<AfspraakOverzichtViewProps> {

	constructor(props: AfspraakOverzichtViewProps) {
		super(props)
		const state = store.getState()
		if (state.pendingUpdates && state.pendingUpdates.moetDaglijstNogVerversen) {
			vernieuwAfsprakenDaglijst()
		}
	}

	handleScan(gescandeData: string): void {
		gescandeData = gescandeData.replace("http:
		const afspraken = store.getState().afsprakenById
		afspraken.forEach((afspraak: Afspraak) => {
			if (String(afspraak.uitnodigingsNr) === gescandeData) {
				if (afspraak.vanafDatum !== vandaagISO()) {
					return
				}
				navigeerNaarClientAfspraak(afspraak)
			}
		})
	}

	render(): JSX.Element {
		return <div className="afspraaklijst">
			<BarcodeReader minimaleLengte={9} onScan={this.handleScan}/>
			<Row>
				<Col md={6}><h6>Gepland
					({this.props.nietAfgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6></Col>
				{this.props.daglijstDatum === vandaagISO() &&
					<Col md={6}><i className="fa fa-refresh float-right" id={"daglijst-refresh"} aria-hidden="true"
								   onClick={(): void => {
									   vernieuwAfsprakenDaglijst(GEFORCEERD_DAGLIJST_OPHALEN)
								   }}/></Col>}
			</Row>
			<AfspraakLijstView afspraken={this.props.nietAfgerondeTijdSlots} clienten={this.props.clienten}
							   emptyText="Er zijn geen geplande afspraken."/>
			<h6>Afgerond ({this.props.afgerondeTijdSlots.filter(value => value instanceof Afspraak).length})</h6>
			<AfspraakLijstView afspraken={this.props.afgerondeTijdSlots} clienten={this.props.clienten}
							   emptyText="Er zijn geen afgeronde afspraken."/>
		</div>
	}

}

export const navigeerNaarClientAfspraak = (afspraak: Afspraak): void => {
	if (!store.getState().autorisatie.onderzoeken) {
		navigateToClientgegevens(store.dispatch, afspraak.clientId, afspraak.id)
	} else {
		switch (afspraak.status) {
			case "INGESCHREVEN":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Vorige onderzoeken")
				break
			case "ONDERZOEK":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Visuele inspectie")
				break
			case "SIGNALEREN":
				navigateToOnderzoek(store.dispatch, afspraak.clientId, afspraak.id, "Signaleren")
				break
			default:
				navigateToClientgegevens(store.dispatch, afspraak.clientId, afspraak.id)
		}
	}
}
