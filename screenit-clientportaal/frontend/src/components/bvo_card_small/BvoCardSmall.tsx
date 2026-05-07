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
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam, BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import styles from "./BvoCardSmall.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import classNames from "classnames"

import VerticalDividerComponent from "../vectors/VerticalDividerComponent"

export type BvoCardSmallProps = {
	bvo: Bevolkingsonderzoek;
}

const BvoCardSmall = (props: BvoCardSmallProps) => {
	const bvoNaam = BevolkingsonderzoekNaam[props.bvo]

	return (
		<div className={classNames(styles.bvoCard, BevolkingsonderzoekStyle[props.bvo])}>
			<VerticalDividerComponent className={styles.verticalRectangle}/>

			<span className={bvoStyle.bvoText}>Bevolkingsonderzoek</span>
			<h1 className={styles.bvoNaam}>{bvoNaam}</h1>
		</div>
	)
}
export default BvoCardSmall
