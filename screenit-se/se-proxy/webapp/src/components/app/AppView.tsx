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
import React, {KeyboardEvent} from "react"
import {BrowserRouter} from "react-router"
import TabbarContainer from "./TabbarContainer"
import type {Session} from "../../datatypes/Session"
import {ToastContainer} from "react-toastify"
import "react-toastify/dist/ReactToastify.css"
import {DEFAULT_TOAST_TIMEOUT} from "../../util/ToastUtil"
import WerkstationkiezerContainer from "../login/WerkstationkiezerContainer"
import {Mammograaf} from "../../datatypes/Mammograaf"
import {store} from "../../Store"
import LoginContainer from "../login/LoginContainer"

export type AppProps = {
	session?: Session;
	mammograafId?: number;
	mammografenById: Map<number, Mammograaf>;
};

const AppView = (props: AppProps): JSX.Element => {
	const environmentInfo = store.getState().environmentInfo
	return <div onKeyDown={onKeyPressed}>
		{!props.session ? <div>
			<LoginContainer/>
		</div> : props.mammograafId === undefined && props.mammografenById.size && environmentInfo && (environmentInfo.environment === "Test" || environmentInfo.environment === "PAT") ?
			<WerkstationkiezerContainer/> : <BrowserRouter>
				<div className="app">
					<TabbarContainer/>
				</div>
			</BrowserRouter>}
		<ToastContainer autoClose={DEFAULT_TOAST_TIMEOUT} hideProgressBar position={"top-center"} icon={false}/>
	</div>
}

const onKeyPressed = (e: KeyboardEvent<HTMLDivElement>): boolean => {
	return !(e.ctrlKey && e.code === "KeyJ")
}

export default AppView
