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
import HeaderView, {HeaderViewDispatchProps, HeaderViewStateProps} from "./HeaderView"
import {logout} from "../../restclient/AuthenticatieRestclient"
import {RootState, store} from "../../Store"

const mapStateToProps = (state: RootState): HeaderViewStateProps => {
	return state.session ? {
		aangemeld: true,
		gebruikersnaam: state.session.gebruikersnaam,
		displayName: state.session.displayName,
		seCode: state.session.seCode,
		seNaam: state.session.seNaam,
		huidigeMammograaf: state.huidigeMammograafId ? state.mammografenById.get(state.huidigeMammograafId) : undefined,
		online: state.online,
		isTestOmgeving: state.environmentInfo ? state.environmentInfo.environment === "Test" : false,
	} : {
		aangemeld: false,
		isTestOmgeving: state.environmentInfo ? state.environmentInfo.environment === "Test" : false,
		online: state.online,
	}
}

const mapDispatchToProps = (): HeaderViewDispatchProps => ({
	afmelden(): void {
		console.log(`Uitloggen via afmeldknop: ${store.getState().session?.yubikeyIdentificatie}`)
		logout(store.getState().session?.yubikeyIdentificatie)
	},
})

const HeaderContainer = connect(mapStateToProps, mapDispatchToProps)(HeaderView)
export default HeaderContainer
