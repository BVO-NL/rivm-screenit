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
import {connect} from "react-redux"
import WerkstationkiezerView, {WerkstationkiezerProps} from "./WerkstationkiezerView"
import {calcSort} from "../../util/Util"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): WerkstationkiezerProps => {
	return {
		mammografen: Array.from(state.mammografenById.values()).sort((m1, m2) => calcSort(m1.aeTitle, m2.aeTitle)),
	}
}

const WerkstationkiezerContainer = connect(mapStateToProps)(WerkstationkiezerView)
export default WerkstationkiezerContainer
