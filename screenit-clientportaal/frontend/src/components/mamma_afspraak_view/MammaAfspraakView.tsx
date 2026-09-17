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
import React, {FC} from "react"
import {useSelector} from "react-redux"
import {selectMammaAfspraakOptie} from "../../selectors/MammaAfspraakSelectors"
import AfspraakView from "../afspraak_view/AfspraakView"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"
import {useWizardStap} from "../wizard_indicator/WizardIndicatorContext"
import {useNavigate} from "react-router"
import SpanWithHtml from "../span/SpanWithHtml"
import styles from "../../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakBevestigingsWizard.module.scss"

export type MammaAfspraakViewProps = {
	tekst: string
}

const MammaAfspraakView: FC<MammaAfspraakViewProps> = (props: MammaAfspraakViewProps) => {
	const afspraakOptie = useSelector(selectMammaAfspraakOptie)!
	const huidigeStap = useWizardStap()
	const navigate = useNavigate()

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
		<>
			<SpanWithHtml className={styles.infoText} value={props.tekst}/>
			<AfspraakView adres={afspraakOptie.adres} postcode={afspraakOptie.postcode} plaats={afspraakOptie.plaats} datumTijd={afspraakOptie.datumTijd}
						  andereAfspraakKiezen={() => andereAfspraakKiezen("wijzigenAfspraakGeklikt")} magWijzigen={true}/>
		</>
	)
}

export default MammaAfspraakView
