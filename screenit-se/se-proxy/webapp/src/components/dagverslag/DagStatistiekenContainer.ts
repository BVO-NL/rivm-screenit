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
import {connect} from "react-redux"
import DagStatistiekenView, {DagStatistiekenViewProps} from "./DagStatistiekenView"
import {RootState} from "../../Store"
import {getIfExists} from "../../util/MapUtil"
import {getAantalNietBeeindigdeAfsprakenMetStatus, getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus, getTotaalAfsprakenDag} from "../../selectors/AfspraakSelectors"

const mapStateToProps = (state: RootState): DagStatistiekenViewProps => {
	return {
		dagverslag: getIfExists(state.dagverslag, state.daglijstDatum),
		aantalVerwacht: getAantalNietBeeindigdeAfsprakenMetStatus(state, "VERWACHT"),
		aantalIngeschreven: getAantalNietBeeindigdeAfsprakenMetStatus(state, "INGESCHREVEN"),
		aantalOnderzoek: getAantalNietBeeindigdeAfsprakenMetStatus(state, "ONDERZOEK"),
		aantalSignaleren: getAantalNietBeeindigdeAfsprakenMetStatus(state, "SIGNALEREN"),
		aantalAfgerond: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "AFGEROND"),
		aantalOnderbroken: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "ONDERBROKEN"),
		aantalOnvolledig: getAantalOpDagBeeindigdeAfsprakenMetOnderzoekStatus(state, "ONVOLLEDIG"),
		aantalTotaal: getTotaalAfsprakenDag(state),
	}
}

const DagStatistiekenContainer = connect(mapStateToProps)(DagStatistiekenView)
export default DagStatistiekenContainer
