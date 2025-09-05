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
import {Nav, Spinner, TabContent, TabPane} from "reactstrap"
import ClientgegevensContainer from "../clientgegevens/ClientgegevensContainer"
import DaglijstView from "../daglijst/DaglijstView"
import TabView from "../generic/TabView"
import type {SubPagina, Tab} from "../../datatypes/Navigation"
import {isClientgegevensTab, isConnectiestatusTab, isDaglijstTab, isDagverslagTab, isKwaliteitsopnameTab, isOnderzoeksTab} from "../../datatypes/Navigation"
import OnderzoekContainer from "../onderzoek/OnderzoekContainer"
import type {Afspraak} from "../../datatypes/Afspraak"
import ErrorView from "./ErrorView"
import type {ErrorDto} from "../../datatypes/ErrorDto"
import DagverslagContainer from "../dagverslag/DagverslagContainer"
import AkkoordPopupContainer from "../generic/popup/AkkoordPopupContainer"
import KwaliteitsopnameContainer from "../kwaliteitsopname/KwaliteitsopnameContainer"
import ConnectieStatusContainer from "../connectiestatus/ConnectieStatusContainer"

export type TabbarViewStateProps = {
	activeTab: Tab;
	afspraak?: Afspraak;
	daglijstTabClickable: boolean;
	clientgegevensTabClickable: boolean;
	onderzoekTabClickable: boolean;
	dagverslagTabClickable: boolean;
	kwaliteitsopnameTabClickable: boolean;
	connectiestatusTabClickable: boolean;
	error?: ErrorDto;
	magOnderzoeken: boolean;
	subPagina?: SubPagina;
	klaarMetDataLaden: boolean;
	daglijstDatum: string;
	huidigeMammograafId?: number;
	online: boolean;
	bezigMetKwaliteitsopnameVolgnr?: number;
};

export type TabbarViewDispatchProps = {
	onClickTab: (tab: Tab, props: TabbarViewStateProps) => void;
}

export default class TabbarView extends React.Component<TabbarViewStateProps & TabbarViewDispatchProps> {
	render(): JSX.Element {
		const activeTab = this.props.activeTab

		if (!this.props.klaarMetDataLaden && activeTab !== "Connectiestatus") {
			return <div>
				<Spinner className={"login-spinner"}/>
				<div className={"jumbotron text-center"}>Bezig met het laden van de daglijst</div>
			</div>
		}

		const clickTab = (tab: Tab): void => this.props.onClickTab(tab, this.props)

		return this.props.error ? <ErrorView error={this.props.error}/> : <div>
			<AkkoordPopupContainer/>
			<Nav tabs>
				<TabView name={"Daglijst"} activeTabName={activeTab.toString()} onClick={clickTab}
						 clickable={this.props.daglijstTabClickable}/>
				<TabView name={"Cliëntgegevens"} activeTabName={activeTab.toString()} onClick={clickTab}
						 clickable={this.props.clientgegevensTabClickable}/>
				{this.props.magOnderzoeken ?
					<TabView name={"Onderzoek"} activeTabName={activeTab.toString()} onClick={clickTab}
							 clickable={this.props.onderzoekTabClickable}/> : null}
				<TabView name={"Dagverslag"} activeTabName={activeTab.toString()} onClick={clickTab}
						 clickable={this.props.dagverslagTabClickable}/>
				<TabView name={"Kwaliteitsopname"} activeTabName={activeTab.toString()} onClick={clickTab}
						 clickable={this.props.kwaliteitsopnameTabClickable}/>
				<TabView name={"Connectiestatus"} activeTabName={activeTab.toString()} onClick={clickTab}
						 clickable={this.props.connectiestatusTabClickable}/>
			</Nav>
			<TabContent activeTab={this.props.activeTab}>
				<TabPane tabId="Daglijst">
					{isDaglijstTab(this.props.activeTab) && <DaglijstView/>}
				</TabPane>
				<TabPane tabId="Cliëntgegevens">
					{isClientgegevensTab(this.props.activeTab) && <ClientgegevensContainer/>}
				</TabPane>
				<TabPane tabId="Onderzoek">
					{isOnderzoeksTab(this.props.activeTab) && <OnderzoekContainer/>}
				</TabPane>
				<TabPane tabId="Dagverslag">
					{isDagverslagTab(this.props.activeTab) && <DagverslagContainer/>}
				</TabPane>
				<TabPane tabId="Kwaliteitsopname">
					{isKwaliteitsopnameTab(this.props.activeTab) && <KwaliteitsopnameContainer/>}
				</TabPane>
				<TabPane tabId="Connectiestatus">
					{isConnectiestatusTab(this.props.activeTab) && <ConnectieStatusContainer/>}
				</TabPane>
			</TabContent>
		</div>
	}

}
