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
import classNames from "classnames"
import styles from "./WizardIndicator.module.scss"
import {useWizardStap} from "./WizardIndicatorContext"

export type WizardIndicatorProps = {
	stappen: { label: string, url: string }[]
	className: string
}
const WizardIndicator = (props: WizardIndicatorProps) => {
	const huidigeStap = useWizardStap()

	function getStepClass(step: number) {
		return classNames(
			styles.step,
			{
				[styles.stepActive]: huidigeStap === step + 1,
				[styles.stepDone]: huidigeStap > step + 1,
				[styles.stepInactive]: huidigeStap < step + 1,
			},
		)
	}

	return (
		<div className={classNames(styles.stepsContainer, props.className)}>
			{
				props.stappen.map((stap, index) =>
					<div className={getStepClass(index)} key={index}>
						<div className={styles.label}>{stap.label}</div>
						<div>
							<div className={styles.circle}>{index + 1}</div>
						</div>
						{index < props.stappen.length - 1 && <div className={styles.line}></div>}
					</div>,
				)
			}
		</div>
	)
}

export default WizardIndicator
