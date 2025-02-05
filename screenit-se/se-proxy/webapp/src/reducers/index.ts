/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {combineReducers} from "redux"
import AfsprakenReducer from "./AfspraakReducer"
import DaglijstDatumReducer from "./DaglijstDatumReducer"
import DagverslagReducer from "./DagverslagReducer"
import NietAfgeslotenVanafReducer from "./NietAfgeslotenVanafReducer"
import SeGebruikersReducer from "./SeGebruikersReducer"
import SessionReducer from "./SessionReducer"
import ClientReducer from "./ClientReducer"
import NavigationReducer from "./NavigationReducer"
import PlanningReducer from "./PlanningReducer"
import VisueleInspectieReducer from "./VisueleInspectieReducer"
import EnvironmentInfoReducer from "./EnvironmentInfoReducer"
import AutorisatieReducer from "./AutorisatieReducer"
import FormReducer from "./FormReducer"
import HuisartsenReducer from "./HuisartsenReducer"
import UpdateReducer from "./UpdateReducer"
import WijzigingenReducer from "./WijzigingenReducer"
import BezigMetKwaliteitsopnameVolgnrReducer from "./BezigMetKwaliteitsopnameVolgnrReducer"
import ImsReducer from "./ImsReducer"
import SignalerenReducer from "./SignalerenReducer"
import ZorginstellingenReducer from "./ZorginstellingenReducer"
import OnderzoekReducer from "./OnderzoekReducer"
import ErrorReducer from "./ErrorReducer"
import MammografenReducer from "./MammografenReducer"
import PopupReducer from "./PopupReducer"
import ConnectionReducer from "./ConnectionReducer"
import MammograafReducer from "./MammograafReducer"
import DubbeleInstantieReducer from "./DubbeleInstantieReducer"
import OpgehaaldeDagenReducer from "./OpgehaaldeDagenReducer"
import ConnectieStatusReducer from "./ConnectieStatusReducer"
import MammografenStatusReducer from "./MammografenStatusReducer"
import LoginStatusReducer from "./LoginStatusReducer"
import WebsocketStatusReducer from "./WebsocketStatusReducer"

const seReducers = combineReducers({
	afsprakenById: AfsprakenReducer,
	daglijstDatum: DaglijstDatumReducer,
	dagverslag: DagverslagReducer,
	nietAfgeslotenVanaf: NietAfgeslotenVanafReducer,
	seGebruikers: SeGebruikersReducer,
	clientenById: ClientReducer,
	planning: PlanningReducer,
	session: SessionReducer,
	navigation: NavigationReducer,
	visueleInspectieAfbeeldingByAfspraakId: VisueleInspectieReducer,
	formsByFormId: FormReducer,
	environmentInfo: EnvironmentInfoReducer,
	autorisatie: AutorisatieReducer,
	onderzoekByAfspraakId: OnderzoekReducer,
	signaleringByAfspraakId: SignalerenReducer,
	huisartsenById: HuisartsenReducer,
	zorginstellingen: ZorginstellingenReducer,
	error: ErrorReducer,
	pendingUpdates: UpdateReducer,
	mammografenById: MammografenReducer,
	huidigeMammograafId: MammograafReducer,
	popup: PopupReducer,
	heeftWijzigingen: WijzigingenReducer,
	online: ConnectionReducer,
	activeStudyForIms: ImsReducer,
	bezigMetKwaliteitsopnameVolgnr: BezigMetKwaliteitsopnameVolgnrReducer,
	dubbeleInstantie: DubbeleInstantieReducer,
	loginStatus: LoginStatusReducer,
	opgehaaldeDagen: OpgehaaldeDagenReducer,
	connectieStatus: ConnectieStatusReducer,
	mammografenStatus: MammografenStatusReducer,
	websocketStatus: WebsocketStatusReducer,
})

export default seReducers
