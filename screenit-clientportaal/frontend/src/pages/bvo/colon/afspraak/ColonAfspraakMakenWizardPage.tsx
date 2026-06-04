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
import ColonAfspraakWizard, {useAfspraakWizardContext} from "./ColonAfspraakWizard"
import {FC, JSX} from "react"
import ColonAfspraakZoekenComponent from "../../../../components/colon_afspraak_zoeken/ColonAfspraakZoekenComponent"
import {VrijSlotZonderKamer} from "../../../../datatypes/VrijSlotZonderKamer"
import {useThunkDispatch} from "../../../../index"
import {setColonVrijSlotZonderKamerAction} from "../../../../actions/ColonDossierAction"
import {useNavigate} from "react-router"
import SpanWithHtml from "../../../../components/span/SpanWithHtml"
import styles from "./ColonAfspraakWizard.module.scss"
import properties from "./ColonAfspraakWizard.json"
import classNames from "classnames"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {NavLink} from "react-bootstrap"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"
import {useSelector} from "react-redux"
import {selectAfspraakTypeDigitaal, selectIntakeafspraakType} from "../../../../selectors/ColonSelectors"

const ColonAfspraakWizardMakenPage = (): JSX.Element => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const intakeafspraakType = useSelector(selectIntakeafspraakType)
	const digitaleIntake = useSelector(selectAfspraakTypeDigitaal)
	const {openAfsluitenPopup} = useAfspraakWizardContext()

	function setGekozenAfspraak(afspraak: VrijSlotZonderKamer): void {
		dispatch(setColonVrijSlotZonderKamerAction({...afspraak, type: intakeafspraakType!}))

		datadogService.stuurEvent(
			"volgendeGeklikt",
			AnalyticsCategorie.AFSPRAAK_VERZETTEN,
			{stap: 3, type: intakeafspraakType})

		navigate(digitaleIntake ? "/colon/afspraak/contact/" : "/colon/afspraak/overzicht/")
	}

	return (
		<div>
			<SpanWithHtml className={styles.infoText} value={digitaleIntake ? properties.afspraak_maken.description.digitaal : properties.afspraak_maken.description.op_locatie}/>
			<ColonAfspraakZoekenComponent onAfspraakGeselecteerd={afspraak => setGekozenAfspraak(afspraak)}/>
			<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
				<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT} onClick={() => navigate("/colon/afspraak/uw-gezondheid/")}
				        label={properties.afspraak_maken.button.vorige}/>
				<div className={styles.knoppenRechts}>
					<NavLink onClick={openAfsluitenPopup} className={styles.andereOptie}>
						{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
				</div>
			</div>
		</div>
	)
}

const WrappedColonAfspraakMakenWizardPage: FC = () => (
	<ColonAfspraakWizard>
		<ColonAfspraakWizardMakenPage/>
	</ColonAfspraakWizard>
)

export default WrappedColonAfspraakMakenWizardPage
