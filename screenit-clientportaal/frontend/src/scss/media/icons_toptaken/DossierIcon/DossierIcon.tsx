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
import {FC} from "react"
import styles from "./DossierIcon.module.scss"

const DossierIcon: FC = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <g>
                <path className={styles.st0} d="M2.5,40.5V8.5h13l4,5h25v27H2.5z"/>
                <line className={styles.st0} x1="2.5" y1="19.5" x2="44.5" y2="19.5"/>
            </g>
        </svg>
    )
}

export default DossierIcon
