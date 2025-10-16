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
import {Component, JSX} from "react"
import DatumkiezerContainer from "../daglijst/DatumkiezerContainer"
import DagproductieContainer from "./DagproductieContainer"
import DagStatistiekenContainer from "./DagStatistiekenContainer"
import DagSynchronisatieContainer from "./DagSynchronisatieContainer"
import DagAfrondstatusContainer from "./DagAfrondstatusContainer"
import AutorisatieButton from "../generic/AutorisatieButton"

export type DagverslagViewStateProps = {
	afsprakenDoorvoerenDisabled: boolean;
	doorvoerenFeedback: string;
	magOnderzoeken: boolean;
	online: boolean;
};

export type DagverslagViewDispatchProps = {
	showAfsprakenDoorvoerenPopup: (afsprakenDoorvoerenDisabled: boolean, doorvoerenFeedback: string) => void;
}

export default class DagverslagView extends Component<DagverslagViewStateProps & DagverslagViewDispatchProps> {
	render(): JSX.Element {
		const clickAfsprakenDoorvoeren = (): void => this.props.showAfsprakenDoorvoerenPopup(this.props.afsprakenDoorvoerenDisabled, this.props.doorvoerenFeedback)

		return <div className="dagverslag-lijst">
			<div className="row row-dagverslag-top">
				<div className="col-3">
					<AutorisatieButton id="dagAfsluitenButton" label={"Dag afsluiten"} online={this.props.online}
									   className={this.props.afsprakenDoorvoerenDisabled ? "disabled" : ""}
									   heeftRecht={this.props.magOnderzoeken} rechtNaam={"Onderzoek starten op SE."}
									   onClick={clickAfsprakenDoorvoeren}/>
				</div>
				<div className="col-9">
					<div className="daglijst-blokken-rechts">
						<div className="dagverslag-datumkiezer">
							<DatumkiezerContainer/>
						</div>
					</div>
				</div>
			</div>
			<DagproductieContainer/>
			<div className="row row-no-gutters">
				<div className="col-6 col-no-gutters-left">
					<DagStatistiekenContainer/>
				</div>
				<div className="col-6 col-no-gutters-right">
					<DagSynchronisatieContainer/>
					<DagAfrondstatusContainer/>
				</div>
			</div>
		</div>
	}

}
