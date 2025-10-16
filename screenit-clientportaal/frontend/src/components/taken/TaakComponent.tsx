/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import {ReactNode} from "react"
import classNames from "classnames"
import styles from "./TaakComponent.module.scss"
import {Bevolkingsonderzoek, BevolkingsonderzoekStyle, BevolkingsonderzoekToptaakStyle} from "../../datatypes/Bevolkingsonderzoek"
import bvoStyle from "../BvoStyle.module.scss"
import {useSelectedBvo} from "../../utils/Hooks"
import {RoutePath} from "../../routes/routes"
import {useNavigate} from "react-router"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"

export type TaakComponentProps = {
	tekst: string,
	link: RoutePath,
	icon?: ReactNode,
}

const TaakComponent = (props: TaakComponentProps): ReactNode => {
	const selectedBvo = useSelectedBvo()
	const navigate = useNavigate()

	const stuurDatadogEventEnNavigeer = (): void => {
		if (selectedBvo === Bevolkingsonderzoek.MAMMA) {
			datadogService.stuurEvent(
				" secundaireActietegelGeklikt",
				AnalyticsCategorie.MAMMA,
				{naam: props.tekst},
			)
		}
		navigate(props.link)
	}

	return (
		<div className={classNames(styles.topTaak, selectedBvo && BevolkingsonderzoekStyle[selectedBvo])}
			 onClick={stuurDatadogEventEnNavigeer}>
			<div className={classNames(styles.icon, BevolkingsonderzoekToptaakStyle[selectedBvo!])}>{props.icon}</div>
			<span className={bvoStyle.bvoText}>Ik wil graag</span>
			<br/>
			<span>{props.tekst}</span>
		</div>
	)
}

export default TaakComponent
