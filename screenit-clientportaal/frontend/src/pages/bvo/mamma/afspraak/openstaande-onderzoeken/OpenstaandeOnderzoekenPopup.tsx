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
import BasePopup from "../../../../../components/popup/BasePopup"
import {NavLink} from "react-bootstrap"
import classNames from "classnames"
import styles from "./OpenstaandeOnderzoekenPopup.module.scss"
import {useSelectedBvo} from "../../../../../utils/Hooks"
import {Bevolkingsonderzoek, BevolkingsonderzoekStyle} from "../../../../../datatypes/Bevolkingsonderzoek"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import type {OpenstaandeUitnodiging} from "../../../../../datatypes/uitnodiging/OpenstaandeUitnodiging"
import BvoCardSmall from "../../../../../components/bvo_card_small/BvoCardSmall"
import Button from "../../../../../components/input/Button"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import properties from "./OpenstaandeOnderzoekenPopupProperties.json"
import {useNavigate} from "react-router"

export type OpenstaandeOnderzoekenPopupProps = {
	openstaandeOnderzoeken?: OpenstaandeUitnodiging[],
}

export const OpenstaandeOnderzoekenPopup = (props: OpenstaandeOnderzoekenPopupProps) => {
	const bvo = useSelectedBvo()
	const navigate = useNavigate()

	const heeftOpenstaandeColon = props.openstaandeOnderzoeken?.some(uitnodiging => uitnodiging.bevolkingsonderzoekType === Bevolkingsonderzoek.COLON) ?? false
	const heeftOpenstaandeCervix = props.openstaandeOnderzoeken?.some(uitnodiging => uitnodiging.bevolkingsonderzoekType === Bevolkingsonderzoek.CERVIX) ?? false

	const naarOnderzoeken = (): void => {
		navigate("/openstaande-onderzoeken/")
	}

	const naarHome = (): void => {
		navigate("/mamma")
	}

	return (
		<BasePopup
			title={getString(properties.titel)}
			description={getString(properties.omschrijving)}>
			<div>
				<div className={classNames(BevolkingsonderzoekStyle[bvo!], styles.huisartsDiv)}>
					{heeftOpenstaandeColon ? <BvoCardSmall bvo={Bevolkingsonderzoek.COLON}/> : null}
					{heeftOpenstaandeCervix ? <BvoCardSmall bvo={Bevolkingsonderzoek.CERVIX}/> : null}

					<p>
						{getString(properties.uitleg.regel1)}
						<br/>
						{getString(properties.uitleg.regel2)}
					</p>
				</div>
				<div className={styles.overigeOnderzoekenKnoppen}>
					<Button
						label={getString(properties.knoppen.primair)}
						onClick={() => {
							datadogService.stuurEvent("overigeOnderzoekenGeklikt", AnalyticsCategorie.MAMMA_AFSPRAAK)
							naarOnderzoeken()
						}}
						displayArrow={ArrowType.ARROW_RIGHT}/>
					<NavLink
						onClick={() => {
							datadogService.stuurEvent("naarHomeGeklikt", AnalyticsCategorie.MAMMA_AFSPRAAK)
							naarHome()
						}}
						className={styles.andereOptie}>
						{getString(properties.knoppen.secundair)}
					</NavLink>
				</div>
			</div>
		</BasePopup>
	)
}
