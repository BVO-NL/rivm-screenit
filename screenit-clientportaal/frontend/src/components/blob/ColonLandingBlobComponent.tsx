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
import BlobComponent from "./BlobComponent"
import {ReactNode} from "react"
import {useSelectedBvo} from "../../utils/Hooks"
import classNames from "classnames"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import bvoStyle from "../BvoStyle.module.scss"
import styles from "./TextBlobComponent.module.scss"
import SpanWithHtml from "../span/SpanWithHtml"
import {ColonIntakeafspraakType} from "../../datatypes/colon/ColonIntakeafspraakType"
import {weergaveColonDigitaleIntakeWeek, weergaveColonIntake} from "../../utils/DateUtil"
import {getVolledigeAdresString} from "../../utils/AdresUtil"
import {splitAdresString} from "../../utils/StringUtil"
import properties from "./ColonLandingBlobComponent.json"
import BvoLandingBlobComponent from "./BvoLandingBlobComponent"
import {ColonIntakeafspraakDto} from "../../datatypes/colon/ColonIntakeafspraakDto"

export type ColonLandingBlobComponentProps = {
	intakeAfspraak: ColonIntakeafspraakDto
}

const ColonLandingBlobComponent = ({intakeAfspraak}: ColonLandingBlobComponentProps): ReactNode => {
	const selectedBvo = useSelectedBvo()

	if (intakeAfspraak.type === ColonIntakeafspraakType.DIGITAAL) {
		return (
			<BlobComponent>
				<div className={classNames(BevolkingsonderzoekStyle[selectedBvo!])}>
					<div className={styles.titelBox}>
						<span className={classNames(styles.titel, bvoStyle.bvoText)}>{properties.digitaal.titel}</span>
						<SpanWithHtml className={styles.tekstTitel}
						              value={weergaveColonDigitaleIntakeWeek(new Date(intakeAfspraak.vanaf))}/>
					</div>
					<div className={styles.adresBox}>
						<span className={classNames(styles.titel, bvoStyle.bvoText)}>{properties.digitaal.locatieLabel}</span>
						<br/>
						<SpanWithHtml className={styles.adresLocatie} value={intakeAfspraak.naamIntakelocatie}/>
						<div className={styles.extraTekst}>{properties.digitaal.contactTekst}</div>
					</div>
				</div>
			</BlobComponent>
		)
	}

	const locatieTekst = `${intakeAfspraak.naamIntakelocatie}<br>${splitAdresString(getVolledigeAdresString(intakeAfspraak.adres))}`
	return (
		<BvoLandingBlobComponent afspraakMoment={weergaveColonIntake(intakeAfspraak.vanaf)}
		                         afspraakLocatie={locatieTekst}/>
	)
}

export default ColonLandingBlobComponent
