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
import BasePage from "../../../BasePage"
import {BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import properties from "./ColonAfspraakWizard.json"
import {getString} from "../../../../utils/TekstPropertyUtil"
import React, {createContext, ReactNode, useContext, useState} from "react"
import WizardIndicator from "../../../../components/wizard_indicator/WizardIndicator"
import styles from "./ColonAfspraakWizard.module.scss"
import {WizardIndicatorProvider} from "../../../../components/wizard_indicator/WizardIndicatorContext"
import {useSelector} from "react-redux"
import {selectIntakeAfspraak, selectIntakeafspraakType} from "../../../../selectors/ColonSelectors"
import {ColonIntakeafspraakType} from "../../../../datatypes/colon/ColonIntakeafspraakType"
import ColonAfspraakWizardAfsluitenPopup from "./ColonAfspraakWizardAfsluitenPopup"

type AfspraakWizardContextType = {
	openAfsluitenPopup: () => void
}

const AfspraakWizardContext = createContext<AfspraakWizardContextType | undefined>(undefined)

export const useAfspraakWizardContext = (): AfspraakWizardContextType => {
	const context = useContext(AfspraakWizardContext)
	if (context === undefined) {
		throw new Error("useAfspraakWizardContext moet binnen een ColonAfspraakWizard gebruikt worden")
	}
	return context
}

export type AfspraakMakenBasePageProps = {
	children: ReactNode,
}

const ColonAfspraakWizard = (props: AfspraakMakenBasePageProps): React.JSX.Element => {
	const intakeafspraakType = useSelector(selectIntakeafspraakType)
	const isDigitaal = intakeafspraakType === ColonIntakeafspraakType.DIGITAAL
	const intakeAfspraak = useSelector(selectIntakeAfspraak)
	const heeftAfspraak = !!intakeAfspraak && !(
		intakeAfspraak.afspraakAfgezegd || intakeAfspraak.andereIntakelocatieOpVerzoekClient
	)
	const [toonBevestigingPopup, setToonBevestigingPopup] = useState(false)

	const wizardStappen = [
		{url: "intake", label: "Intake"},
		{url: "uw-gezondheid", label: "Uw gezondheid"},
		{url: "kiezen", label: "Kies afspraak"},
		{url: "overzicht", label: "Overzicht"},
	]

	if (isDigitaal) {
		wizardStappen.splice(3, 0, {url: "contact", label: "Contact"})
	}

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.COLON}
				  title={getString(heeftAfspraak ? properties.page.title.afspraak_verzetten : properties.page.title.afspraak_maken)}
				  toonBlob={false}>
			<WizardIndicatorProvider stappen={wizardStappen}>
				<WizardIndicator stappen={wizardStappen} className={styles.wizardIndicator}/>
				<AfspraakWizardContext.Provider value={{openAfsluitenPopup: () => setToonBevestigingPopup(true)}}>
					{props.children}
				</AfspraakWizardContext.Provider>
			</WizardIndicatorProvider>
			{toonBevestigingPopup &&
				<ColonAfspraakWizardAfsluitenPopup onSluiten={() => setToonBevestigingPopup(false)}/>
			}
		</BasePage>
	)
}

export default ColonAfspraakWizard
