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
import MammaAfspraakBevestigingsWizard from "./MammaAfspraakBevestigingsWizard"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import properties from "./MammaAfspraakBevestigingsWizard.json"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import Button from "../../../../../components/input/Button"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import {NavLink} from "react-bootstrap"
import {useNavigate} from "react-router"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import {maakAfspraak} from "../../../../../api/MammaAfspraakMakenThunkAction"
import {useThunkDispatch} from "../../../../../index"
import {useSelector} from "react-redux"
import {selectMammaAfspraakOptie} from "../../../../../selectors/MammaAfspraakSelectors"
import {showToast} from "../../../../../utils/ToastUtil"
import classNames from "classnames"
import {setMammaAfspraakBevestigingsoptieReduxAction} from "../../../../../actions/MammaAfspraakAction"
import {ToastMessageType} from "../../../../../datatypes/toast/ToastMessage"
import React, {FC, useState} from "react"
import BasePopup from "../../../../../components/popup/BasePopup"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import AfspraakView from "../../../../../components/afspraak_view/AfspraakView"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"

const MammaAfspraakBevestigenPage: FC = () => {
	const dispatch = useThunkDispatch()
	const afspraakOptie = useSelector(selectMammaAfspraakOptie)!
	const [afspraakBevestigingMislukt, setAfspraakBevestigingMislukt] = useState(false)
	const [toonBevestigingsPopup, setToonBevestigingsPopup] = useState(false)
	const navigate = useNavigate()
	const huidigeStap = useWizardStap()

	const afspraakMaken = async (): Promise<void> => {
		datadogService.stuurEvent(
			"afspraakBevestigd",
			AnalyticsCategorie.MAMMA_AFSPRAAK,
			{
				stap: huidigeStap,
			},
		)

		try {
			await dispatch(maakAfspraak(afspraakOptie))
			navigate("/mamma/afspraak/bevestiging-selectie")
		} catch (error: any) {
			if (error.response.data === "tijd.niet.beschikbaar") {
				dispatch(setMammaAfspraakBevestigingsoptieReduxAction(undefined))
				setAfspraakBevestigingMislukt(true)
			} else {
				showToast(properties.afspraak_maken.toast.geen_wijzigingen, properties.afspraak_maken.toast.error.algemeen, ToastMessageType.ERROR)
			}
		}
	}

	const andereAfspraakKiezen = (eventNaam?: string): void => {
		if (eventNaam) {
			datadogService.stuurEvent(
				eventNaam,
				AnalyticsCategorie.MAMMA_AFSPRAAK,
				{
					...(huidigeStap !== 0 ? {stap: huidigeStap} : {}),
				},
			)
		}
		navigate("/mamma/afspraak")
	}

	return (
		<div>
			<SpanWithHtml className={styles.infoText} value={properties.page.description.afspraak_maken}/>
			<AfspraakView afspraakOptie={afspraakOptie} andereAfspraakKiezen={() => andereAfspraakKiezen("wijzigenAfspraakGeklikt")} magWijzigen={true}/>
			<div className={styles.bevestigenForm}>
				{afspraakBevestigingMislukt ? <Button label={properties.afspraak_maken.button.andere_afspraak}
													  onClick={() => andereAfspraakKiezen("wijzigenAfspraakGeklikt")}
													  displayArrow={ArrowType.ARROW_RIGHT}/> :
					<>
						<Button label={getString(properties.afspraak_maken.button.volgende)}
								displayArrow={ArrowType.ARROW_RIGHT}
								onClick={afspraakMaken}/>
						<NavLink onClick={() => setToonBevestigingsPopup(true)} className={styles.andereOptie}>
							{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
					</>
				}
			</div>

			{
				toonBevestigingsPopup &&
				<BasePopup title={properties.afspraak_maken.annuleren_bevestigingspopup.title}
						   description={properties.afspraak_maken.annuleren_bevestigingspopup.description}>
					<div className={classNames(styles.bevestigenForm, styles.inPopup)}>
						<Button label={properties.afspraak_maken.annuleren_bevestigingspopup.button_bevestigen}
								displayArrow={ArrowType.ARROW_RIGHT}
								onClick={() => setToonBevestigingsPopup(false)}/>
						<NavLink onClick={() => andereAfspraakKiezen("afsluitenAfspraakMaken")}>
							{properties.afspraak_maken.annuleren_bevestigingspopup.button_annuleren}</NavLink>
					</div>
				</BasePopup>
			}
		</div>
	)
}

const WrappedMammaAfspraakBevestigenPage: FC = () => (
	<MammaAfspraakBevestigingsWizard>
		<MammaAfspraakBevestigenPage/>
	</MammaAfspraakBevestigingsWizard>
)

export default WrappedMammaAfspraakBevestigenPage
