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
import {useNavigate} from "react-router"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import properties from "./MammaAfspraakBevestigingsWizard.json"
import {BevestigingsType} from "../../../../../datatypes/BevestigingsType"
import {selectMammaAfspraakBevestigingsoptie} from "../../../../../selectors/MammaAfspraakSelectors"
import {useSelector} from "react-redux"
import MammaAfspraakBevestigingsWizard from "./MammaAfspraakBevestigingsWizard"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import Button from "../../../../../components/input/Button"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import classNames from "classnames"
import AfsluitenLink from "../../../../../components/afsluiten_link/AfsluitenLink"
import {FC, useEffect, useState} from "react"
import {OpenstaandeOnderzoekenPopup} from "../openstaande-onderzoeken/OpenstaandeOnderzoekenPopup"
import {State} from "../../../../../datatypes/State"
import {getOpenstaandeUitnodigingen} from "../../../../../api/OpenstaandeUitnodigingenThunkAction"
import {useThunkDispatch} from "../../../../../index"
import MammaAfspraakView from "../../../../../components/mamma_afspraak_view/MammaAfspraakView"
import HuisartsView from "../../../../../components/huisarts_view/HuisartsView"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"

const MammaAfspraakOverzichtPage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const huidigeStap = useWizardStap()
	const afspraakBevestiging = useSelector(selectMammaAfspraakBevestigingsoptie)!
	const [toonOpenstaandePopup, setToonOpenstaandePopup] = useState(false)
	const openstaandeOnderzoeken = useSelector((state: State) => state.client.openstaandeUitnodigingen)
	const huidigeHuisarts = useSelector((state: State) => state.client.mammaDossier.huisartsHuidigeRonde)

	useEffect(() => {
		dispatch(getOpenstaandeUitnodigingen())
	}, [])

	const afsluiten = (): void => {
		datadogService.stuurEvent("afspraakoverzichtBekeken", AnalyticsCategorie.MAMMA_AFSPRAAK, {
			stap: huidigeStap,
		})
		if (openstaandeOnderzoeken?.length > 0) {
			setToonOpenstaandePopup(true)
		} else {
			navigate("/mamma")
		}
	}

	return (
		<div>
			{toonOpenstaandePopup && <OpenstaandeOnderzoekenPopup
				openstaandeOnderzoeken={openstaandeOnderzoeken}
			/>}

			<MammaAfspraakView tekst={properties.bevestiging.afspraak_bijschrift}/>
			{
				huidigeHuisarts && <><SpanWithHtml value={properties.bevestiging.huisarts_bijschrift}/>
					<HuisartsView huisarts={huidigeHuisarts!} andereHuisartsKiezen={() => navigate("/mamma/afspraak/uw-huisarts?wijzig=true")}/>
				</>
			}
			<SpanWithHtml value={maakOmschrijving()}/>

			<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
				<Button lightStyle={true}
						displayArrow={ArrowType.ARROW_LEFT}
						onClick={() => navigate("/mamma/afspraak/uw-huisarts/")}
						label={properties.afspraak_maken.button.vorige}/>
				<div className={styles.knoppenRechts}>
					<Button label={properties.afspraak_maken.button.afronden}
							onClick={afsluiten}
							displayArrow={ArrowType.ARROW_RIGHT}/>
					<AfsluitenLink/>
				</div>
			</div>
		</div>
	)

	function maakOmschrijving(): string {
		let tekst = ""
		if (BevestigingsType.MAIL === afspraakBevestiging.bevestigingsType) {
			tekst = tekst.concat(properties.bevestiging.mail)
		}
		if (BevestigingsType.BRIEF === afspraakBevestiging.bevestigingsType) {
			tekst = tekst.concat(properties.bevestiging.brief)
		}
		if (afspraakBevestiging.wilHerinneringsSms) {
			tekst = tekst.concat(properties.bevestiging.sms)
		}
		tekst = tekst.concat(properties.bevestiging.meenemen)
		return tekst
	}
}

const WrappedMammaAfspraakOverzichtPage: FC = () => (
	<MammaAfspraakBevestigingsWizard>
		<MammaAfspraakOverzichtPage/>
	</MammaAfspraakBevestigingsWizard>
)
export default WrappedMammaAfspraakOverzichtPage
