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
import type {AlleenOnlineButtonProps} from "./AlleenOnlineButton"
import AlleenOnlineButton from "./AlleenOnlineButton"

export type AutorisatieButtonProps = AlleenOnlineButtonProps & {
	heeftRecht: boolean;
	rechtNaam: string;
};

export default class AutorisatieButton extends AlleenOnlineButton<AutorisatieButtonProps> {

	static defaultProps = {
		online: true,
		className: "float-right",
	}

	render(): JSX.Element {
		const enabled = this.props.heeftRecht && this.props.online
		return this.getButton(enabled)
	}

	getPopoverBody: any = () => {
		return this.props.popovertekst ? this.props.popovertekst : !this.props.heeftRecht ? `Hiervoor heeft u niet de benodigde autorisatie ${this.props.rechtNaam}` : !this.props.online ? this.getPopoverSeOfflineText() : null
	}
}
