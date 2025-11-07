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
import * as React from "react"
import {AfspraakOptie} from "../../../../../../datatypes/mamma/AfspraakOptie"
import BasePopup from "../../../../../../components/popup/BasePopup"
import {Col, NavLink, Row} from "react-bootstrap"
import classNames from "classnames"
import {formatDateText, formatDateTime, formatTime} from "../../../../../../utils/DateUtil"
import bvoStyles from "../../../../../../components/BvoStyle.module.scss"
import styles from "./MammaAfspraakMakenWizardModuleStyles.module.scss"
import {useSelectedBvo} from "../../../../../../utils/Hooks"
import {BevolkingsonderzoekStyle} from "../../../../../../datatypes/Bevolkingsonderzoek"
import Button from "../../../../../../components/input/Button"
import {useThunkDispatch} from "../../../../../../index"
import {getString} from "../../../../../../utils/TekstPropertyUtil"
import VerticalDividerComponent from "../../../../../../components/vectors/VerticalDividerComponent"
import properties from "./MammaAfspraakMakenWizardModuleProperties.json"
import {ArrowType} from "../../../../../../components/vectors/ArrowIconComponent"
import {maakAfspraak} from "../../../../../../api/MammaAfspraakMakenThunkAction"
import {AfspraakBevestigingOpties} from "../../../../../../datatypes/mamma/AfspraakBevestigingOpties"
import {useSelector} from "react-redux"
import {State} from "../../../../../../datatypes/State"
import {showToast} from "../../../../../../utils/ToastUtil"
import {ToastMessageType} from "../../../../../../datatypes/toast/ToastMessage"
import datadogService from "../../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../../datatypes/AnalyticsCategorie"

export type MammaAfspraakMakenPopupProps = {
	afspraak: AfspraakOptie,
	isBevestigingsPopup: boolean,
	onFailure?: () => void,
	onNext?: () => void,
	onAndereAfspraakKiezen: () => void,
	children?: React.ReactNode,
	setAfspraakBevestiging?: React.Dispatch<React.SetStateAction<AfspraakBevestigingOpties | undefined>>,
}

const MammaAfspraakMakenPopup = (props: MammaAfspraakMakenPopupProps) => {
	const bvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const afspraakOptie = props.afspraak
	const client = useSelector((state: State) => state.client)

	const afspraakMaken = () => {
		datadogService.stuurEvent(
			"mammaAfspraakOptieBevestigd",
			AnalyticsCategorie.MAMMA_AFSPRAAK,
			{
				client: client.persoon.id,
				datum: formatDateTime(afspraakOptie.datumTijd),
				standplaatsPeriode: afspraakOptie.standplaatsPeriodeId,
			},
		)

		dispatch(maakAfspraak(bvo, afspraakOptie)).then(
			(response) => {
				props.setAfspraakBevestiging!(new AfspraakBevestigingOpties(response, afspraakOptie))
				props.onNext!()
			},
		).catch((error) => {
			if (error.response.data === "tijd.niet.beschikbaar") {
				props.onFailure && props.onFailure()
			} else {
				showToast(getString(properties.afspraak_maken.toast.geen_wijzigingen), getString(properties.afspraak_maken.toast.error.algemeen), ToastMessageType.ERROR)
			}
		})
	}

	const andereAfspraakKiezen = () => {
		datadogService.stuurEvent(
			"mammaAfspraakAndereOptieZoeken",
			AnalyticsCategorie.MAMMA_AFSPRAAK,
			{
				client: client.persoon.id,
				datum: formatDateTime(afspraakOptie.datumTijd),
				standplaatsPeriode: afspraakOptie.standplaatsPeriodeId,
				redenTijdNietMeerBeschikbaar: !props.isBevestigingsPopup,
			},
		)
		props.onAndereAfspraakKiezen()
	}

	return (
		<BasePopup title={props.isBevestigingsPopup ? getString(properties.afspraak_maken.bevestiging.title) : getString(properties.afspraak_maken.failure.title)}
				   description={props.isBevestigingsPopup ? getString(properties.afspraak_maken.bevestiging.description) : getString(properties.afspraak_maken.failure.description)}>
			<div>
				<div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.afspraakDiv)}>
					<VerticalDividerComponent className={styles.verticalRectangle} heightSubtraction={15}/>
					<Row className={styles.afspraakGegevensRow}>
						<Col sm={6}>
							<span className={classNames(bvoStyles.bvoText)}>{getString(properties.afspraak_maken.headers.datumtijd)}</span>
							<span>{formatDateText(props.afspraak.datumTijd)}</span>
							<span>{`${formatTime(props.afspraak.datumTijd)} uur`}</span>
						</Col>
						<Col sm={6} className={styles.locatieColumn}>
							<span className={classNames(bvoStyles.bvoText)}>{getString(properties.afspraak_maken.headers.locatie)}</span>
							<span>{props.afspraak.adres}</span>
							<span>{`${props.afspraak.postcode} ${props.afspraak.plaats}`}</span>
						</Col>
					</Row>
				</div>
				<div className={styles.bevestigenForm}>
					{props.isBevestigingsPopup &&
						<div>
							<Button label={getString(properties.afspraak_maken.button.bevestigen)}
									displayArrow={ArrowType.ARROW_RIGHT}
									onClick={afspraakMaken}/>
							<NavLink onClick={andereAfspraakKiezen} className={styles.andereOptie}>
								{getString(properties.afspraak_maken.button.andere_afspraak)}</NavLink>
						</div>}
					{!props.isBevestigingsPopup &&
						<Button label={getString(properties.afspraak_maken.button.andere_afspraak)}
								onClick={andereAfspraakKiezen}
								displayArrow={ArrowType.ARROW_RIGHT}/>}
				</div>
				{props.children}
			</div>
		</BasePopup>)
}

export default MammaAfspraakMakenPopup
