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
import classNames from "classnames"

import styles from "./BvoHistorieCard.module.scss"
import {Bevolkingsonderzoek, BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import {useSelectedBvo} from "../../utils/Hooks"
import {Col, Row} from "react-bootstrap"
import {getString} from "../../utils/TekstPropertyUtil"
import {formatDate, formatTime} from "../../utils/DateUtil"
import SpanWithHtml from "../span/SpanWithHtml"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import properties from "./BvoHistorieCard.json"

export type BvoHistorieCardProps = {
	datumTijd: Date
	tekstKey: string
	extraParameters: string[]
}

const BvoHistorieCard = (props: BvoHistorieCardProps) => {
	const selectedBvo = useSelectedBvo()

	return (
		<Row className={classNames(styles.historyCard, BevolkingsonderzoekStyle[selectedBvo!])}>
			<VerticalDividerComponent className={styles.verticalRectangle}/>
			<Col md={2} className={classNames(styles.datumTijd)}>
				<span className={classNames(styles.datum)}>{formatDate(props.datumTijd)}</span>
				<span>{formatTime(props.datumTijd)} uur</span>
			</Col>
			<Col md={10} className={styles.tekst}>
				<SpanWithHtml value={getHistorieTekst(props.tekstKey)}/>
			</Col>
		</Row>
	)

	function getHistorieTekst(tekstKey: string): string {
		if ("EENMALIGE_AFMELDING" === tekstKey) {
			return selectedBvo === Bevolkingsonderzoek.MAMMA ? getString(properties["EENMALIGE_AFMELDING_BK"]) : getString(properties["EENMALIGE_AFMELDING_DK_BMHK"])
		} else {
			const tekst = props.tekstKey as keyof typeof properties
			return getString(properties[tekst], props.extraParameters)
		}
	}

}

export default BvoHistorieCard
