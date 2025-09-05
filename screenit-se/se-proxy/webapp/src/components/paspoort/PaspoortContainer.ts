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
import type {PaspoortProps} from "./PaspoortView"
import PaspoortView from "./PaspoortView"
import {ligtTussenData, vandaagDate} from "../../util/DateUtil"
import {RootState} from "../../Store"
import {Client} from "../../datatypes/Client"
import {Afspraak} from "../../datatypes/Afspraak"

export type PaspoortContainerProps = {
	client: Client;
	afspraak: Afspraak;
}

const mapStateToProps = (state: RootState, ownProps: PaspoortContainerProps): PaspoortProps => {
	const tijdelijkAdres = ownProps.client.tijdelijkAdres

	if (tijdelijkAdres && ligtTussenData(vandaagDate(), tijdelijkAdres.startDatum, tijdelijkAdres.eindDatum)) {
		return {
			...ownProps,
			tijdelijkAdresValue: "Er is een tijdelijk adres beschikbaar",
		}
	} else {
		return {
			...ownProps,
			tijdelijkAdresValue: "Nee",
		}
	}
}

const PaspoortContainer = connect(mapStateToProps)(PaspoortView)
export default PaspoortContainer
