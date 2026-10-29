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
import React, { FC } from "react"
import styles from "./Button.module.scss"
import classNames from "classnames"
import ArrowIconComponent, {ArrowType} from "../vectors/ArrowIconComponent"

export type ButtonProps = {
	className?: string
	label: string,
	disableButton?: boolean
	displayArrow?: ArrowType,
	arrowBeforeLabel?: boolean,
	onClick: () => void,
	lightStyle?: boolean
	testId?: string
	id?: string
}

const Button: FC<ButtonProps> = (props) => {
	return (
		<button
			type={"submit"}
			id={props.id}
			className={classNames(styles.style, props.className, props.lightStyle ? styles.light : styles.normal)}
			onClick={(event) => {
				event.preventDefault()
				if (!props.disableButton) {
					props.onClick()
				}
			}}
		>
            <span>
                {!props.arrowBeforeLabel && props.label}
				{props.displayArrow && <ArrowIconComponent className={props.arrowBeforeLabel ? styles.icon_left : styles.icon_right} type={props.displayArrow}/>}
				{props.arrowBeforeLabel && props.label}
            </span>
		</button>
	)
}

export default Button
