/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-frontend
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
import * as React from "react"
import {Navigate, RouteProps} from "react-router"
import {useAppSelector, useAppThunkDispatch} from "../index"
import {AuthenticationScope} from "../state/datatypes/enums/AuthenticationScope"
import {Recht} from "../state/datatypes/enums/Recht"
import LocatieVerificatieMeldingComponent from "../components/locatie/verificatie/LocatieVerificatieMeldingComponent"
import {createClearStateAction} from "../state"

type PrivateRouteParams = RouteProps & {
	scope: AuthenticationScope;
	recht?: Recht;
	component: React.ComponentType<any>;
}

const PrivateRoute: React.FC<PrivateRouteParams> = ({scope, recht, component: Component}) => {
	const auth = useAppSelector(state => state.auth)
	const locatieVerificatie = useAppSelector(state => state.locatieVerificatie)
	const user = useAppSelector(state => state.user)
	const authenticationLoading = useAppSelector(state => state.authenticationLoading)
	const dispatch = useAppThunkDispatch()

	if (authenticationLoading) {
		return <div><span>Inloggen...</span></div>
	} else if (auth?.scope === scope && (!recht || (user?.rollen.includes(recht)))) {
		return <div>
			{(scope === AuthenticationScope.LOGIN && locatieVerificatie && locatieVerificatie.length > 0) && <LocatieVerificatieMeldingComponent/>}
			<Component/>
		</div>
	} else if (user?.rollen.includes(Recht.ROLE_OVEREENKOMST)) {
		return <Navigate replace to={"/overeenkomst"}/>
	} else if (auth?.scope === AuthenticationScope.REGISTREREN) {
		return <Navigate replace to={"/registreren/voltooien"}/>
	} else {
		dispatch(createClearStateAction())
		return <Navigate replace to={"/login"}/>
	}
}

export default PrivateRoute
