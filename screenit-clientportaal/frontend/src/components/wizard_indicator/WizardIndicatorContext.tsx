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
import {createContext, ReactElement, ReactNode, useContext} from "react"
import {useLocation} from "react-router"

type WizardIndicatorContextType = {
	huidigeStap: number
}

const WizardIndicatorContext = createContext<WizardIndicatorContextType | undefined>(undefined)

export type WizardIndicatorProviderProps = {
	stappen: { label: string, url: string }[]
	children: ReactNode
}

export const WizardIndicatorProvider = (props: WizardIndicatorProviderProps): ReactElement => {
	const location = useLocation()

	const index = props.stappen.findIndex(stap => location.pathname.includes(stap.url))
	const huidigeStap = index !== -1 ? index + 1 : 0

	return (
		<WizardIndicatorContext.Provider value={{huidigeStap}}>
			{props.children}
		</WizardIndicatorContext.Provider>
	)
}

export const useWizardStap = (): number => {
	const context = useContext(WizardIndicatorContext)
	if (context === undefined) {
		return 0
	}
	return context.huidigeStap
}
