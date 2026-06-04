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
import SpanWithHtml from "../../../../components/span/SpanWithHtml"
import styles from "./ColonAfspraakWizard.module.scss"
import properties from "./ColonAfspraakWizard.json"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"
import {useNavigate} from "react-router"
import {FC} from "react"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {NavLink} from "react-bootstrap"
import ColonAfspraakWizard, {useAfspraakWizardContext} from "./ColonAfspraakWizard"
import classNames from "classnames"
import {useThunkDispatch} from "../../../../index"
import {getBvoBaseUrl} from "../../../../utils/UrlUtil"
import {Bevolkingsonderzoek} from "../../../../datatypes/Bevolkingsonderzoek"
import {useSelector} from "react-redux"
import {selectAfspraakTypeDigitaal, selectGeselecteerdSlot, selectHeraanmelding, selectIntakeAfspraak} from "../../../../selectors/ColonSelectors"
import {afspraakVerplaatsen, nieuweAfspraak} from "../../../../api/ColonAfspraakMakenThunkAction"
import AfspraakView from "../../../../components/afspraak_view/AfspraakView"
import bvoStyles from "../../../../components/BvoStyle.module.scss"
import {selectPersoon} from "../../../../selectors/ClientSelectors"

const ColonAfspraakOverzichtPage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const heraanmelding = useSelector(selectHeraanmelding)
	const vrijSlot = useSelector(selectGeselecteerdSlot)
	const huidigeIntakeAfspraak = useSelector(selectIntakeAfspraak)!
	const isAfspraakDigitaal = useSelector(selectAfspraakTypeDigitaal)
	const persoon = useSelector(selectPersoon)
	const {openAfsluitenPopup} = useAfspraakWizardContext()

	const afspraak: { adres: string, postcode: string, plaats: string, datumTijd: Date, naamIntakelocatie: string } = vrijSlot !== null ? {
		adres: vrijSlot.adres,
		postcode: vrijSlot.postcode,
		plaats: vrijSlot.plaats,
		datumTijd: vrijSlot.startTijd,
		naamIntakelocatie: vrijSlot.ziekenhuis,
	} : {
		adres: `${huidigeIntakeAfspraak.adres.straat} ${huidigeIntakeAfspraak.adres.huisnummer}`,
		postcode: huidigeIntakeAfspraak.adres.postcode ?? "",
		plaats: huidigeIntakeAfspraak.adres.plaats ?? "",
		datumTijd: huidigeIntakeAfspraak.vanaf,
		naamIntakelocatie: huidigeIntakeAfspraak.naamIntakelocatie,
	}

	const andereAfspraakKiezen = (eventNaam?: string): void => {
		if (eventNaam) {
			datadogService.stuurEvent(
				eventNaam,
				AnalyticsCategorie.COLON_AFSPRAAK,
			)
		}
		navigate("/colon/afspraak/kiezen/")
	}

	const afspraakMaken = async (): Promise<void> => {
		if (!vrijSlot) {
			return
		}

		if (heraanmelding) {
			await dispatch(nieuweAfspraak(vrijSlot))
		} else {
			if (isAfspraakDigitaal) {
				datadogService.stuurEvent(
					"bevestigFysiekeAfspraak",
					AnalyticsCategorie.AFSPRAAK_VERZETTEN,
					{stap: 4},
				)
			} else {
				datadogService.stuurEvent(
					"bevestigDigitaleAfspraak",
					AnalyticsCategorie.AFSPRAAK_VERZETTEN,
					{stap: 5},
				)
			}
			await dispatch(afspraakVerplaatsen(vrijSlot))
		}
		navigate(getBvoBaseUrl(Bevolkingsonderzoek.COLON))
	}

	return (
		<div>
			<SpanWithHtml className={styles.infoText} value={isAfspraakDigitaal ? properties.overzicht.description : properties.page.description}/>
			<div>{getString(properties.overzicht.afspraakgegevens)}</div>
			<AfspraakView naamIntakelocatie={afspraak.naamIntakelocatie}
			              adres={afspraak.adres}
			              postcode={afspraak.postcode}
			              plaats={afspraak.plaats}
			              datumTijd={afspraak.datumTijd}
			              digitaleIntake={isAfspraakDigitaal}
			              andereAfspraakKiezen={() => andereAfspraakKiezen("wijzigenAfspraakGeklikt")} magWijzigen={true}/>

			{isAfspraakDigitaal && (
				<>
					<div>{getString(properties.overzicht.contactgegevens)}</div>
					<div className={styles.contactGegevensContainer}>
						<div className={classNames(bvoStyles.bvoText, styles.contactGegevensHeader)}>Contactgegevens</div>
						<div>{persoon.emailadres}</div>
						<div>{persoon.telefoonnummer1}</div>
					</div>

					<p>
						<SpanWithHtml
							value={getString(properties.overzicht.digitaal_intake_info, [`${afspraak.naamIntakelocatie} (${afspraak.adres} ${afspraak.postcode} ${afspraak.plaats})`])}/>
						<SpanWithHtml value={getString(properties.overzicht.digitaal_intake_link)}/>
						<NavLink onClick={() => andereAfspraakKiezen("wijzigenAfspraakGeklikt")} className={styles.wijzigenLink}>Wijzigen</NavLink>
					</p>
					<p>
						<SpanWithHtml value={getString(properties.overzicht.digitaal_intake_uitleg_knoppen)}/>
					</p>
				</>
			)}
			<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
				<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT}
				        onClick={() => navigate(isAfspraakDigitaal ? "/colon/afspraak/contact/" : "/colon/afspraak/kiezen/")}
				        label={properties.afspraak_maken.button.vorige}/>
				<div className={styles.knoppenRechts}>
					<Button label={getString(properties.afspraak_maken.button.bevestigen)}
					        displayArrow={ArrowType.ARROW_RIGHT}
					        onClick={afspraakMaken}/>
					<NavLink onClick={openAfsluitenPopup} className={styles.andereOptie}>
						{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
				</div>
			</div>
		</div>
	)
}

const WrappedColonAfspraakOverzichtPage: FC = () => (
	<ColonAfspraakWizard>
		<ColonAfspraakOverzichtPage/>
	</ColonAfspraakWizard>
)

export default WrappedColonAfspraakOverzichtPage
