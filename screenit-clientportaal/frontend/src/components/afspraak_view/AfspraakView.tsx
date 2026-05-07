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
import {AfspraakOptie} from "../../datatypes/mamma/AfspraakOptie"
import {formatDateText, formatTime} from "../../utils/DateUtil"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import classNames from "classnames"
import {useSelectedBvo} from "../../utils/Hooks"
import styles from "./AfspraakView.module.scss"
import {Col, NavLink, Row} from "react-bootstrap"
import bvoStyles from "../BvoStyle.module.scss"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./AfspraakView.json"
import {JSX} from "react"

export type AfspraakViewProps = {
	afspraakOptie: AfspraakOptie,
	andereAfspraakKiezen?: () => void
	magWijzigen: boolean

}

const AfspraakView = (props: AfspraakViewProps): JSX.Element => {
	const bvo = useSelectedBvo()!

	return (
		<div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.afspraakDiv)}>
			<VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={15}/>
			<Row className={styles.afspraakGegevensRow}>
				<Col sm={5}>
					<span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.datumtijd)}</span>
					<span>{formatDateText(props.afspraakOptie.datumTijd)}</span>
					<span>{`${formatTime(props.afspraakOptie.datumTijd)} uur`}</span>
				</Col>
				<Col sm={5} className={styles.locatieColumn}>
					<span className={classNames(bvoStyles.bvoText)}>{getString(properties.headers.locatie)}</span>
					<span>{props.afspraakOptie.adres}</span>
					<span>{`${props.afspraakOptie.postcode} ${props.afspraakOptie.plaats}`}</span>
				</Col>
				{props.magWijzigen && <Col sm={2} className={styles.wijzigenColumn}>
					<NavLink onClick={props.andereAfspraakKiezen}>
						{getString(properties.button.wijzigen)}
					</NavLink>
				</Col>}
			</Row>
		</div>
	)
}

export default AfspraakView
