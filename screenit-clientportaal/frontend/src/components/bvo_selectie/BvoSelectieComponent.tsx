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
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../datatypes/Bevolkingsonderzoek"
import classNames from "classnames"
import {useSelector} from "react-redux"
import {useLocation, useNavigate} from "react-router"
import {selectBehoortTotCervixDoelgroep, selectBehoortTotColonDoelgroep, selectBehoortTotMammaDoelgroep} from "../../selectors/BvoSelectors"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"

type BvoSelectieComponentProps = {
	className?: string
}

const BvoSelectieComponent = (props: BvoSelectieComponentProps) => {
	const navigate = useNavigate()
	const location = useLocation()

	const behoortTotMammaDoelgroep = useSelector(selectBehoortTotMammaDoelgroep)
	const behoortTotColonDoelgroep = useSelector(selectBehoortTotColonDoelgroep)
	const behoortTotCervixDoelgroep = useSelector(selectBehoortTotCervixDoelgroep)

	let analyticsCategorie = null
	if (location.pathname === "/") {
		analyticsCategorie = AnalyticsCategorie.LANDINGSPAGINA
	} else if (location.pathname === "/mamma") {
		analyticsCategorie = AnalyticsCategorie.MAMMA
	}

	const stuurDatadogEventEnNavigeer = (bevolkingsonderzoekNaam: string, path: string) => {
		if (analyticsCategorie) {
			datadogService.stuurEvent(bevolkingsonderzoekNaam, analyticsCategorie)
		}
		navigate(path)
	}

	return (
		<Row className={classNames(styles.bvoSelectie, props.className)}>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.MAMMA} clickable={behoortTotMammaDoelgroep} onClick={() =>
					stuurDatadogEventEnNavigeer(BevolkingsonderzoekNaam.MAMMA, "/mamma")}/>
			</Col>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.CERVIX} clickable={behoortTotCervixDoelgroep} onClick={() =>
					stuurDatadogEventEnNavigeer(BevolkingsonderzoekNaam.CERVIX, "/cervix")}/>
			</Col>
			<Col lg={4}>
				<BvoCard bvo={Bevolkingsonderzoek.COLON} clickable={behoortTotColonDoelgroep} onClick={() =>
					stuurDatadogEventEnNavigeer(BevolkingsonderzoekNaam.COLON, "/colon")}/>
			</Col>
		</Row>
	)
}

export default BvoSelectieComponent
