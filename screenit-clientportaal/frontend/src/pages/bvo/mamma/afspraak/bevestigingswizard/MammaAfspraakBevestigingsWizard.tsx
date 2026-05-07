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
import BasePage from "../../../../BasePage"
import {BevolkingsonderzoekNaam} from "../../../../../datatypes/Bevolkingsonderzoek"
import properties from "./MammaAfspraakBevestigingsWizard.json"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import React, {ReactNode, useEffect, useState} from "react"
import WizardIndicator from "../../../../../components/wizard_indicator/WizardIndicator"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import {WizardIndicatorProvider} from "../../../../../components/wizard_indicator/WizardIndicatorContext"
import {useSelector} from "react-redux"
import {selectMammaAfspraakOptie} from "../../../../../selectors/MammaAfspraakSelectors"

export type AfspraakMakenBasePageProps = {
	children: ReactNode,
}

const MammaAfspraakBevestigingsWizard = (props: AfspraakMakenBasePageProps): React.JSX.Element => {
	const alleStappen = [
		{url: "bevestigen", label: "Uw afspraak"},
		{label: "Bevestigen", url: "bevestiging-selectie"},
		{label: "Herinneren", url: "herinnering"},
		{url: "overzicht", label: "Overzicht"},
		{url: "uw-huisarts", label: "Uw huisarts"},
	]

	const [wizardStappen, setWizardStappen] = useState(alleStappen)

	const afspraakOptie = useSelector(selectMammaAfspraakOptie)
	useEffect(() => {
		if (!afspraakOptie?.toonSmsHerinneringOptie) {
			setWizardStappen(alleStappen.filter(stap => stap.url !== "herinnering"))
		} else {
			setWizardStappen(alleStappen)
		}
	}, [afspraakOptie])

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.MAMMA}
				  title={getString(properties.page.title.afspraak_maken)}
				  toonBlob={false}>
			<WizardIndicatorProvider stappen={wizardStappen}>
				<WizardIndicator stappen={wizardStappen} className={styles.wizardIndicator}/>
				{props.children}
			</WizardIndicatorProvider>
		</BasePage>
	)
}

export default MammaAfspraakBevestigingsWizard
