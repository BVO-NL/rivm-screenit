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
import {selectMammaAfspraakBevestigingsoptie, selectMammaAfspraakOptie} from "../../../../../selectors/MammaAfspraakSelectors"
import {useSelector} from "react-redux"
import MammaAfspraakBevestigingsWizard from "./MammaAfspraakBevestigingsWizard"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import Button from "../../../../../components/input/Button"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import classNames from "classnames"
import AfspraakView from "../../../../../components/afspraak_view/AfspraakView"
import AfsluitenLink from "../../../../../components/afsluiten_link/AfsluitenLink"
import React, {FC} from "react"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"

const MammaAfspraakOverzichtPage: FC = () => {
	const navigate = useNavigate()
	const afspraakBevestiging = useSelector(selectMammaAfspraakBevestigingsoptie)!
	const afspraakOptie = useSelector(selectMammaAfspraakOptie)!

	const navigeerNaarVorigePagina = (): void => {
		const url = afspraakBevestiging.toonSmsOptie ? "/mamma/afspraak/herinnering/" : "/mamma/afspraak/bevestiging-selectie/"
		navigate(url)
	}
	const huidigeStap = useWizardStap()

	return (
		<div>
			<SpanWithHtml value={properties.bevestiging.description}/>
			<AfspraakView adres={afspraakOptie.adres} postcode={afspraakOptie.postcode} plaats={afspraakOptie.plaats} datumTijd={afspraakOptie.datumTijd} magWijzigen={false}/>
			<SpanWithHtml value={maakOmschrijving()}/>

			<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
				<Button lightStyle={true}
				        displayArrow={ArrowType.ARROW_LEFT}
				        onClick={navigeerNaarVorigePagina}
				        label={properties.afspraak_maken.button.vorige}/>
				<div className={styles.knoppenRechts}>
					<Button label={properties.afspraak_maken.button.volgende}
					        onClick={() => {
								datadogService.stuurEvent("afspraakoverzichtBekeken", AnalyticsCategorie.MAMMA_AFSPRAAK, {
									stap: huidigeStap,
								})
								navigate("/mamma/afspraak/uw-huisarts")
							}}
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
