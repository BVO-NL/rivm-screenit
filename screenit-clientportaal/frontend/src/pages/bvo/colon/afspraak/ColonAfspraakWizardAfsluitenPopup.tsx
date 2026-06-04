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
import BasePopup from "../../../../components/popup/BasePopup"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {NavLink} from "react-bootstrap"
import classNames from "classnames"
import styles from "./ColonAfspraakWizard.module.scss"
import properties from "./ColonAfspraakWizard.json"
import {JSX} from "react"
import {useNavigate} from "react-router"
import {useSelectedBvo} from "../../../../utils/Hooks"
import {getBvoBaseUrl} from "../../../../utils/UrlUtil"

export type ColonAfspraakWizardAfsluitenPopupProps = {
	onSluiten: () => void
}

const popupProperties = properties.afspraak_maken.annuleren_bevestigingspopup

const ColonAfspraakWizardAfsluitenPopup = (props: ColonAfspraakWizardAfsluitenPopupProps): JSX.Element => {
	const navigate = useNavigate()
	const bvo = useSelectedBvo()

	const afsluitenEnNavigeer = (): void => {
		navigate(getBvoBaseUrl(bvo))
	}

	return (
		<BasePopup title={popupProperties.title}
		           description={popupProperties.description}>
			<div className={classNames(styles.bevestigenForm, styles.inPopup)}>
				<Button label={popupProperties.button_bevestigen}
				        displayArrow={ArrowType.ARROW_RIGHT}
				        onClick={props.onSluiten}/>
				<NavLink onClick={afsluitenEnNavigeer}>
					{popupProperties.button_annuleren}
				</NavLink>
			</div>
		</BasePopup>
	)
}

export default ColonAfspraakWizardAfsluitenPopup
