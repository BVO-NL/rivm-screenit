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
import styles from "./DownloadIcon.module.scss"

const DownloadIcon: FC = () => {
    return (
        <svg version="1.1" id="Layer_1" xmlns="http:
             viewBox="0 0 47 47" xmlSpace="preserve">
            <g>
                <line className={styles.st0} x1="23.5" y1="2.5" x2="23.5" y2="29.5"/>
                <polyline className={styles.st0} points="14.5,20.5 23.5,29.5 32.5,20.5 	"/>
                <polyline className={styles.st0} points="2.5,33.5 2.5,44.5 44.5,44.5 44.5,33.5 	"/>
            </g>
        </svg>
    )
}

export default DownloadIcon
