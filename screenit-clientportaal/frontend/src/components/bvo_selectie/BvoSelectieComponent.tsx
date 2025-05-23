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
import {Col, Row} from "react-bootstrap"
import styles from "./BvoSelectieComponent.module.scss"
import BvoCard from "../bvo_card/BvoCard"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import React from "react"
import classNames from "classnames"
import {useSelector} from "react-redux"
import {useNavigate} from "react-router"
import {selectBehoortTotCervixDoelgroep, selectBehoortTotColonDoelgroep, selectBehoortTotMammaDoelgroep} from "../../selectors/BvoSelectors"

type BvoSelectieComponentProps = {
	className?: string
}

const BvoSelectieComponent = (props: BvoSelectieComponentProps) => {
	const navigate = useNavigate()

	const behoortTotMammaDoelgroep = useSelector(selectBehoortTotMammaDoelgroep)
	const behoortTotColonDoelgroep = useSelector(selectBehoortTotColonDoelgroep)
	const behoortTotCervixDoelgroep = useSelector(selectBehoortTotCervixDoelgroep)

	return (
		<Row className={classNames(styles.bvoSelectie, props.className)}>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.MAMMA} clickable={behoortTotMammaDoelgroep} onClick={() => navigate("/mamma")}/>
			</Col>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.CERVIX} clickable={behoortTotCervixDoelgroep} onClick={() => navigate("/cervix")}/>
			</Col>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.COLON} clickable={behoortTotColonDoelgroep} onClick={() => navigate("/colon")}/>
			</Col>
		</Row>
	)

}

export default BvoSelectieComponent
