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
import React from "react"
import {NavItem, NavLink} from "reactstrap"
import type {Tab} from "../../datatypes/Navigation"

export type TabViewProps = {
	name: Tab;
	activeTabName: string;
	onClick: (tab: Tab) => void;
	clickable: boolean;
};

export default class TabView extends React.Component<TabViewProps> {

	render(): JSX.Element {
		const isActive = this.props.activeTabName === this.props.name
		return <NavItem>
			<NavLink className={isActive ? "active" : this.props.clickable ? "clickable" : "not-clickable"}
					 onClick={(): void => {
						 if (!isActive) {
							 this.props.onClick(this.props.name)
						 }
					 }}>
				{this.props.name}
			</NavLink>
		</NavItem>
	}

}
