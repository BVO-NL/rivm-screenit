/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import classNames from "classnames"
import {useSelectedBvo} from "../../utils/Hooks"
import styles from "./HuisartsView.module.scss"
import {Col, NavLink, Row} from "react-bootstrap"
import bvoStyles from "../BvoStyle.module.scss"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./HuisartsView.json"
import {Huisarts} from "../../datatypes/Huisarts"
import {JSX} from "react"

export type HuisartsViewProps = {
	huisarts: Huisarts,
	andereHuisartsKiezen?: () => void
}

const HuisartsView = (props: HuisartsViewProps): JSX.Element => {
	const bvo = useSelectedBvo()!

	return (
		<div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.huisartsDiv)}>
			<VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={15}/>
			<Row className={styles.huisartsGegevensRow}>
				<Col sm={5}>
					<span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.naam)}</span>
					<span>{props.huisarts.praktijknaam}</span>
					<span>{props.huisarts.voorletters} {props.huisarts.achternaam}</span>
				</Col>
				<Col sm={5} className={styles.locatieColumn}>
					<span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.locatie)}</span>
					<span>{props.huisarts.adres.straat} {props.huisarts.adres.huisnummer}</span>
					<span>{`${props.huisarts.adres.postcode} ${props.huisarts.adres.plaats}`}</span>
				</Col>
				<Col sm={2} className={styles.wijzigenColumn}>
					<NavLink onClick={props.andereHuisartsKiezen}>
						{getString(properties.button.wijzigen)}
					</NavLink>
				</Col>
			</Row>
		</div>
	)
}

export default HuisartsView
